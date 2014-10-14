{-# LANGUAGE FlexibleContexts #-}
module Build where

import Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Exception as Exception
import Control.Monad (forM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Build.Display as Display
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Path
import qualified Utils.Queue as Queue
import TheMasterPlan (ModuleID(ModuleID), Location, BuildSummary, BuildData(..))


data Env = Env
    { maxActiveThreads :: Int
    , numTasks :: Int
    , resultChan :: Chan.Chan Result
    , displayChan :: Chan.Chan Display.Update
    , reverseDependencies :: Map.Map ModuleID [ModuleID]
    }

data State = State
    { currentState :: CurrentState
    , activeThreads :: Set.Set ThreadId
    , readyQueue :: Queue.Queue (ModuleID, Location, Map.Map ModuleID Module.Interface)
    , buildSummary :: BuildSummary
    }

data CurrentState = Wait | Update

data Result
    = Error String
    | Success ModuleID Module.Interface ThreadId


-- HELPERS for ENV and STATE

initEnv :: Int -> Map.Map ModuleID [ModuleID] -> BuildSummary -> IO Env
initEnv numProcessors dependencies summary =
  do  resultChan <- Chan.newChan
      displayChan <- Chan.newChan
      return $ Env {
          maxActiveThreads = numProcessors,
          numTasks = Map.size summary,
          resultChan = resultChan,
          displayChan = displayChan,
          reverseDependencies = reverseGraph dependencies
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
initState summary =
    State {
        currentState = Update,
        activeThreads = Set.empty,
        readyQueue = Queue.fromList (Map.elems readyModules),
        buildSummary = blockedModules
    }
  where
    (readyModules, blockedModules) =
        Map.mapEitherWithKey categorize summary

    categorize name buildData@(BuildData blocking ready location) =
        case blocking of
          [] -> Left (name, location, ready)
          _  -> Right buildData


numIncompleteTasks :: State -> Int
numIncompleteTasks state =
    Set.size (activeThreads state)
    + Queue.size (readyQueue state)
    + Map.size (buildSummary state)


-- PARALLEL BUILDS!!!

build :: Int -> Map.Map ModuleID [ModuleID] -> BuildSummary -> IO ()
build numProcessors dependencies summary =
  do  env <- initEnv numProcessors dependencies summary
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
            Error msg ->
              do  mapM killThread (Set.toList (activeThreads state))
                  Chan.writeChan (displayChan env) (Display.Error msg)

    Update ->
      do  threadIds <-
              forM runNow $ \(name, location, interfaces) ->
                  forkIO (buildModule (resultChan env) name location interfaces)
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
    { currentState = Update
    , activeThreads = Set.delete threadId (activeThreads state)
    , buildSummary = updatedBuildSummary
    , readyQueue = Queue.enqueue (Maybe.catMaybes readyModules) (readyQueue state)
    }
  where
    (updatedBuildSummary, readyModules) =
        List.mapAccumR
            (updateBuildSummary name interface)
            (buildSummary state)
            (Maybe.fromMaybe [] (Map.lookup name (reverseDependencies env)))


updateBuildSummary
    :: ModuleID
    -> Module.Interface
    -> BuildSummary
    -> ModuleID
    -> (BuildSummary, Maybe (ModuleID, Location, Map.Map ModuleID Module.Interface))
updateBuildSummary name interface buildSummary potentiallyFreedModule =
  case Map.lookup potentiallyFreedModule buildSummary of
    Nothing ->
        (buildSummary, Nothing)

    Just (BuildData blocking ready location) ->
          let newReady = Map.insert name interface ready in
          case filter (/= name) blocking of
          [] ->
              ( Map.delete potentiallyFreedModule buildSummary
              , Just (potentiallyFreedModule, location, newReady)
              )

          newBlocking ->
              ( Map.insert
                  potentiallyFreedModule
                  (BuildData newBlocking newReady location)
                  buildSummary
              , Nothing
              )


-- UPDATE - BUILD SOME MODULES

buildModule
    :: Chan.Chan Result
    -> ModuleID
    -> Location
    -> Map.Map ModuleID Module.Interface
    -> IO ()
buildModule completionChan moduleName location interfaces =
    Exception.catch compile recover
  where
    compile =
      do  source <- readFile (Path.fromLocation location)
          let ifaces = Map.mapKeysMonotonic (\(ModuleID name _pkg) -> name) interfaces
          let rawResult = Compiler.compile source ifaces
          result <-
              case rawResult of
                Left errorMsg -> return (Error errorMsg)
                Right interface ->
                  do  threadId <- myThreadId
                      return (Success moduleName interface threadId)

          Chan.writeChan completionChan result

    recover :: Exception.SomeException -> IO ()
    recover msg =
      Chan.writeChan completionChan (Error (show msg))