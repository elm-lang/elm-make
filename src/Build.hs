module Build where

import qualified Control.Concurrent.Chan as Chan
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Build.Queue as Queue
import qualified Crawl.Locations as Locations


type WaitingModules =
    Map.Map Module.Name ([Module.Name], [Module.Interface])

data CurrentState = Wait | Update

data State = State
    { currentState :: CurrentState
    , numActiveThreads :: Int
    , readyQueue :: Queue.Queue
    , waitingModules :: WaitingModules
    }

data Env = Env
    { maxActiveThreads :: Int
    , completions :: Chan.Chan (Module.Name, Module.Interface)
    , locations :: Locations.Locations
    , freeMap :: Map.Map Module.Name [Module.Name]
    }


buildManager :: Env -> State -> IO ()
buildManager env state =
  case currentState state of
    Wait ->
      do  (name, interface) <- Chan.readChan (completions state)
          buildManager env (registerCompletion env state name interface)

    Update ->
      do  forM runNow $ \name ->
              forkIO (buildModule (completions env) (locations env) name)
          buildManager env $
              state
              { numActiveThreads = numActiveThreads state + length runNow
              , readyQueue = runLater
              }
      where
        (runNow, runLater) =
            Queue.dequeue (maxActiveThreads env - numActiveThreads state) queue
    

-- WAIT - REGISTER COMPLETIONS

registerCompletion :: Env -> State -> Module.Name -> Module.Interface -> State
registerCompletion env state name interface =
    state
    { state = Update
    , numActiveThreads = numActiveThreads state - 1
    , waiting = waitingModule
    , readyQueue = Queue.enqueue (Maybe.catMaybes readyModules) (readyQueue state)
    }
  where
    (waitingModule, readyModules) =
        mapAccumR
            (updateWaitingModules name interface)
            (waitingModules state)
            (freeMap env ! name)


updateWaitingModules
    :: Module.Name
    -> Module.Interface
    -> WaitingModules
    -> Module.Name
    -> (WaitingModules, Maybe (Module.Name, [Module.Interface]))
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
              updatedWaitingModules
                  Map.insert
                      potentiallyFreedModule
                      (updatedBlockingModules, interface : interfaces)
                      waitingModules


-- UPDATE - BUILD SOME MODULES

buildModule :: Chan.Chan Module.Name -> Locations.Locations -> Module.Name -> IO ()
buildModule completionChan locations moduleName =
    do  (error "not sure how to build yet") moduleName
        Chan.writeChan completionChan moduleName