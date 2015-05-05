{-# LANGUAGE FlexibleContexts #-}
module Build where

import Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Display
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import qualified Path
import qualified Utils.File as File
import qualified Utils.Queue as Queue
import qualified TheMasterPlan as TMP
import TheMasterPlan
    ( ModuleID, Location, PackageID
    , BuildSummary(BuildSummary), BuildData(..)
    )


data Env = Env
    { maxActiveThreads :: Int
    , numTasks :: Int
    , resultChan :: Chan.Chan Result
    , displayChan :: Chan.Chan Display.Update
    , reverseDependencies :: Map.Map ModuleID [ModuleID]
    , cachePath :: FilePath
    , publicModules :: Set.Set ModuleID
    }

data State = State
    { currentState :: CurrentState
    , activeThreads :: Set.Set ThreadId
    , readyQueue :: Queue.Queue (ModuleID, Location)
    , blockedModules :: Map.Map ModuleID BuildData
    , completedInterfaces :: Map.Map ModuleID Module.Interface
    }

data CurrentState = Wait | Update

data Result
    = Error ModuleID Location String
    | Success ModuleID Module.Interface String ThreadId


-- HELPERS for ENV and STATE

initEnv
    :: Int
    -> FilePath
    -> [ModuleID]
    -> Map.Map ModuleID [ModuleID]
    -> BuildSummary
    -> IO Env
initEnv numProcessors cachePath publicModules dependencies (BuildSummary blocked _completed) =
  do  resultChan <- Chan.newChan
      displayChan <- Chan.newChan
      return $ Env {
          maxActiveThreads = numProcessors,
          numTasks = Map.size blocked,
          resultChan = resultChan,
          displayChan = displayChan,
          reverseDependencies = reverseGraph dependencies,
          cachePath = cachePath,
          publicModules = Set.fromList publicModules
      }


-- reverse dependencies, "who depends on me?"
reverseGraph :: (Ord a) => Map.Map a [a] -> Map.Map a [a]
reverseGraph graph =
    Map.foldrWithKey flipEdges Map.empty graph
  where
    flipEdges name dependencies reversedGraph =
        foldr (insertDependency name) reversedGraph dependencies

    insertDependency name dep reversedGraph =
        Map.insertWith (++) dep [name] reversedGraph


initState :: BuildSummary -> State
initState (BuildSummary blocked completed) =
    State {
        currentState = Update,
        activeThreads = Set.empty,
        readyQueue = Queue.fromList (Map.elems readyModules),
        blockedModules = blockedModules,
        completedInterfaces = completed
    }
  where
    (readyModules, blockedModules) =
        Map.mapEitherWithKey categorize blocked

    categorize name buildData@(BuildData blocking location) =
        case blocking of
          [] -> Left (name, location)
          _  -> Right buildData


numIncompleteTasks :: State -> Int
numIncompleteTasks state =
    Set.size (activeThreads state)
    + Queue.size (readyQueue state)
    + Map.size (blockedModules state)


-- PARALLEL BUILDS!!!

build :: Int -> PackageID -> FilePath -> [ModuleID] -> Map.Map ModuleID [ModuleID] -> BuildSummary -> IO ()
build numProcessors rootPkg cachePath publicModules dependencies summary =
  do  env <- initEnv numProcessors cachePath publicModules dependencies summary
      forkIO (buildManager env (initState summary))
      Display.display (displayChan env) rootPkg 0 (numTasks env)


buildManager :: Env -> State -> IO ()
buildManager env state =
  case currentState state of
    _ | numIncompleteTasks state == 0 ->
          Chan.writeChan (displayChan env) Display.Success

    Wait ->
      do  result <- Chan.readChan (resultChan env)
          case result of
            Success moduleID interface js threadId ->
              do  let cache = cachePath env
                  File.writeBinary (Path.toInterface cache moduleID) interface
                  writeFile (Path.toObjectFile cache moduleID) js
                  Chan.writeChan (displayChan env) (Display.Completion moduleID)
                  buildManager env (registerSuccess env state moduleID interface threadId)

            Error moduleID location msg ->
              do  mapM killThread (Set.toList (activeThreads state))
                  Chan.writeChan (displayChan env) (Display.Error moduleID location msg)

    Update ->
      do  let interfaces = completedInterfaces state
          let compile = buildModule env interfaces
          threadIds <- mapM (forkIO . compile) runNow
          buildManager env $
              state
              { currentState = Wait
              , activeThreads = foldr Set.insert (activeThreads state) threadIds
              , readyQueue = runLater
              }
      where
        (runNow, runLater) =
            Queue.dequeue
                (maxActiveThreads env - Set.size (activeThreads state))
                (readyQueue state)

        progress =
            fromIntegral (numIncompleteTasks state) / fromIntegral (numTasks env)


-- WAIT - REGISTER RESULTS

registerSuccess :: Env -> State -> ModuleID -> Module.Interface -> ThreadId -> State
registerSuccess env state name interface threadId =
    state
    { currentState =
        Update
    , activeThreads =
        Set.delete threadId (activeThreads state)
    , blockedModules =
        updatedBlockedModules
    , readyQueue =
        Queue.enqueue (Maybe.catMaybes readyModules) (readyQueue state)
    , completedInterfaces =
        Map.insert name interface (completedInterfaces state)
    }
  where
    (updatedBlockedModules, readyModules) =
        List.mapAccumR
            (updateBlockedModules name interface)
            (blockedModules state)
            (Maybe.fromMaybe [] (Map.lookup name (reverseDependencies env)))


updateBlockedModules
    :: ModuleID
    -> Module.Interface
    -> Map.Map ModuleID BuildData
    -> ModuleID
    -> (Map.Map ModuleID BuildData, Maybe (ModuleID, Location))
updateBlockedModules name interface blockedModules potentiallyFreedModule =
  case Map.lookup potentiallyFreedModule blockedModules of
    Nothing ->
        (blockedModules, Nothing)

    Just (BuildData blocking location) ->
          case filter (/= name) blocking of
          [] ->
              ( Map.delete potentiallyFreedModule blockedModules
              , Just (potentiallyFreedModule, location)
              )

          newBlocking ->
              ( Map.insert
                  potentiallyFreedModule
                  (BuildData newBlocking location)
                  blockedModules
              , Nothing
              )


-- UPDATE - BUILD SOME MODULES

buildModule
    :: Env
    -> Map.Map ModuleID Module.Interface
    -> (ModuleID, Location)
    -> IO ()
buildModule env interfaces (moduleID, location) =
    Exception.catch compile recover
  where
    (Pkg.Name user project) =
       fst (TMP.packageID moduleID)

    compile =
      do  let path = Path.toSource location
          source <- readFile path
          let ifaces = Map.mapKeysMonotonic TMP.moduleName interfaces
          let (warnings, rawResult) =
                  Compiler.compile user project source ifaces
          result <-
            case rawResult of
              Left errors ->
                let msg = concatMap (Compiler.errorToString path source) errors
                in
                    return (Error moduleID location msg)

              Right (interface, js) ->
                case checkPorts env moduleID interface of
                  Just msg ->
                    return (Error moduleID location msg)

                  Nothing ->
                    do  threadId <- myThreadId
                        return (Success moduleID interface js threadId)

          Chan.writeChan (resultChan env) result

    recover :: Exception.SomeException -> IO ()
    recover msg =
      Chan.writeChan (resultChan env) (Error moduleID location (show msg))


checkPorts :: Env -> ModuleID -> Module.Interface -> Maybe String
checkPorts env moduleID interface =
  case Set.member moduleID (publicModules env) of
    True -> Nothing
    False ->
      case Module.interfacePorts interface of
        [] -> Nothing
        _ -> Just portError


portError :: String
portError =
    concat
    [ "Port Error: ports may only appear in the main module. We do not want ports\n"
    , "    appearing in library code where it adds a non-modular dependency. If I\n"
    , "    import it twice, what does that really mean? This restriction may be\n"
    , "    lifted later."
    ]
