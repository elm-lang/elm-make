module Build where

import Control.Concurrent (ThreadId, myThreadId, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad (forM)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Build.Display as Display
import qualified Elm.Compiler.Module as Module
import qualified Utils.Graph as Graph
import qualified Utils.Queue as Queue


data Env = Env
    { maxActiveThreads :: Int
    , numTasks :: Int
    , resultChan :: Chan.Chan Result
    , displayChan :: Chan.Chan Display.Update
    , freeMap :: Map.Map Module.Name [Module.Name]
    }

initEnv :: Int -> Map.Map Module.Name [Module.Name] -> IO Env
initEnv numProcessors dependencies =
  do  resultChan <- Chan.newChan
      displayChan <- Chan.newChan
      return $ Env {
          maxActiveThreads = numProcessors,
          numTasks = Map.size dependencies,
          resultChan = resultChan,
          displayChan = displayChan,
          freeMap = Graph.reverse dependencies
      }


data State = State
    { currentState :: CurrentState
    , activeThreads :: Set.Set ThreadId
    , readyQueue :: Queue.Queue (Module.Name, [ModuleInterface])
    , waitingModules :: WaitingModules
    }

data ModuleInterface = TODO_IMPLEMENT_ME

type WaitingModules =
    Map.Map Module.Name ([Module.Name], [ModuleInterface])

data CurrentState = Wait | Update

numIncompleteTasks :: State -> Int
numIncompleteTasks state =
    Set.size (activeThreads state)
    + Queue.size (readyQueue state)
    + Map.size (waitingModules state)

data Result
    = Error
    | Success Module.Name ModuleInterface ThreadId


build :: IO ()
build =
  do  displayChan <- Chan.newChan
      Display.display displayChan 0
      buildManager (error "env") (error "state")


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
            Error ->
              do  mapM killThread (Set.toList (activeThreads state))
                  Chan.writeChan (displayChan env) Display.Error

    Update ->
      do  threadIds <-
              forM runNow $ \(name, interfaces) ->
                  forkIO (buildModule (resultChan env) name interfaces)
          Chan.writeChan (displayChan env) (Display.Progress progress)
          buildManager env $
              state
              { activeThreads = foldr Set.insert (activeThreads state) threadIds
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

registerSuccess :: Env -> State -> Module.Name -> ModuleInterface -> ThreadId -> State
registerSuccess env state name interface threadId =
    state
    { currentState = Update
    , activeThreads = Set.delete threadId (activeThreads state)
    , waitingModules = updatedWaitingModules
    , readyQueue = Queue.enqueue (Maybe.catMaybes readyModules) (readyQueue state)
    }
  where
    (updatedWaitingModules, readyModules) =
        List.mapAccumR
            (updateWaitingModules name interface)
            (waitingModules state)
            (freeMap env ! name)


updateWaitingModules
    :: Module.Name
    -> ModuleInterface
    -> WaitingModules
    -> Module.Name
    -> (WaitingModules, Maybe (Module.Name, [ModuleInterface]))
updateWaitingModules name interface waitingModules potentiallyFreedModule =
  case Map.lookup potentiallyFreedModule waitingModules of
    Nothing ->
        (waitingModules, Nothing)

    Just (blockingModules, interfaces) ->
        case filter (/= name) blockingModules of
          [] ->
              ( Map.delete potentiallyFreedModule waitingModules
              , Just (potentiallyFreedModule, interface : interfaces)
              )

          updatedBlockingModules ->
              ( updatedWaitingModules, Nothing )
            where
              updatedWaitingModules =
                  Map.insert
                      potentiallyFreedModule
                      (updatedBlockingModules, interface : interfaces)
                      waitingModules


-- UPDATE - BUILD SOME MODULES

buildModule :: Chan.Chan Result -> Module.Name -> [ModuleInterface] -> IO ()
buildModule completionChan moduleName interfaces =
    do  interface <- (error "not sure how to build yet") moduleName
        threadId <- myThreadId
        Chan.writeChan completionChan (Success moduleName interface threadId)
