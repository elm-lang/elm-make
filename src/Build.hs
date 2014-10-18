{-# LANGUAGE FlexibleContexts #-}
module Build where

import Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Build.Display as Display
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Path
import qualified Utils.File as File
import qualified Utils.Queue as Queue
import TheMasterPlan
    ( ModuleID(ModuleID), Location
    , BuildSummary(BuildSummary), BuildData(..)
    )


data Env = Env
    { maxActiveThreads :: Int
    , numTasks :: Int
    , resultChan :: Chan.Chan Result
    , displayChan :: Chan.Chan Display.Update
    , reverseDependencies :: Map.Map ModuleID [ModuleID]
    , cachePath :: FilePath
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
    = Error ModuleID String
    | Success ModuleID Module.Interface ThreadId


-- HELPERS for ENV and STATE

initEnv :: Int -> FilePath -> Map.Map ModuleID [ModuleID] -> BuildSummary -> IO Env
initEnv numProcessors cachePath dependencies (BuildSummary blocked _completed) =
  do  resultChan <- Chan.newChan
      displayChan <- Chan.newChan
      return $ Env {
          maxActiveThreads = numProcessors,
          numTasks = Map.size blocked,
          resultChan = resultChan,
          displayChan = displayChan,
          reverseDependencies = reverseGraph dependencies,
          cachePath = cachePath
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

build :: Int -> FilePath -> Map.Map ModuleID [ModuleID] -> BuildSummary -> IO ()
build numProcessors cachePath dependencies summary =
  do  env <- initEnv numProcessors cachePath dependencies summary
      forkIO (buildManager env (initState summary))
      Display.display (displayChan env) 0 (numTasks env)


buildManager :: Env -> State -> IO ()
buildManager env state =
  case currentState state of
    _ | numIncompleteTasks state == 0 ->
          Chan.writeChan (displayChan env) Display.Success

    Wait ->
      do  result <- Chan.readChan (resultChan env)
          case result of
            Success name interface threadId ->
              do  Chan.writeChan (displayChan env) (Display.Completion name)
                  buildManager env (registerSuccess env state name interface threadId)
            Error name msg ->
              do  mapM killThread (Set.toList (activeThreads state))
                  Chan.writeChan (displayChan env) (Display.Error name msg)

    Update ->
      do  let interfaces = completedInterfaces state
          let compile = buildModule (resultChan env) (cachePath env) interfaces
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
    :: Chan.Chan Result
    -> FilePath
    -> Map.Map ModuleID Module.Interface
    -> (ModuleID, Location)
    -> IO ()
buildModule completionChan cachePath interfaces (moduleName, location) =
    Exception.catch compile recover
  where
    compile =
      do  source <- readFile (Path.toSource location)
          let ifaces = Map.mapKeysMonotonic (\(ModuleID name _pkg) -> name) interfaces
          let rawResult = Compiler.compile source ifaces
          result <-
              case rawResult of
                Left errorMsg -> return (Error moduleName errorMsg)
                Right interface ->
                  do  threadId <- myThreadId
                      File.writeBinary (Path.toInterface cachePath moduleName) interface 
                      return (Success moduleName interface threadId)

          Chan.writeChan completionChan result

    recover :: Exception.SomeException -> IO ()
    recover msg =
      Chan.writeChan completionChan (Error moduleName (show msg))
