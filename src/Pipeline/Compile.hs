module Pipeline.Compile where

import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs

import qualified BuildManager as BM
import qualified Path
import qualified Report
import qualified Utils.File as File
import qualified Utils.Queue as Queue
import qualified TheMasterPlan as TMP
import TheMasterPlan
    ( CanonicalModule(..), Location, Package
    , BuildGraph(BuildGraph), BuildData(..)
    )


data Env = Env
    { maxActiveThreads :: Int
    , numTasks :: Int
    , resultChan :: Chan.Chan Result
    , reportChan :: Chan.Chan Report.Message
    , docsChan :: Chan.Chan [Docs.Documentation]
    , dependencies :: Map.Map CanonicalModule [CanonicalModule]
    , reverseDependencies :: Map.Map CanonicalModule [CanonicalModule]
    , cachePath :: FilePath
    , exposedModules :: Set.Set CanonicalModule
    , modulesForGeneration :: Set.Set CanonicalModule
    }


data State = State
    { currentState :: CurrentState
    , activeThreads :: Set.Set ThreadId
    , readyQueue :: Queue.Queue (CanonicalModule, Location)
    , blockedModules :: Map.Map CanonicalModule BuildData
    , completedInterfaces :: Map.Map CanonicalModule Module.Interface
    , documentation :: [Docs.Documentation]
    }


data CurrentState = Wait | Update


-- HELPERS for ENV and STATE

initEnv
    :: Int
    -> FilePath
    -> Set.Set CanonicalModule
    -> [CanonicalModule]
    -> Map.Map CanonicalModule [CanonicalModule]
    -> BuildGraph
    -> IO Env
initEnv numProcessors cachePath exposedModules modulesForGeneration dependencies (BuildGraph blocked _completed) =
  do  resultChan <- Chan.newChan
      reportChan <- Chan.newChan
      docsChan <- Chan.newChan
      return $ Env
        { maxActiveThreads = numProcessors
        , numTasks = Map.size blocked
        , resultChan = resultChan
        , reportChan = reportChan
        , docsChan = docsChan
        , dependencies = dependencies
        , reverseDependencies = reverseGraph dependencies
        , cachePath = cachePath
        , exposedModules = exposedModules
        , modulesForGeneration = Set.fromList modulesForGeneration
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


initState :: BuildGraph -> State
initState (BuildGraph blocked completed) =
    State
      { currentState = Update
      , activeThreads = Set.empty
      , readyQueue = Queue.fromList (Map.elems readyModules)
      , blockedModules = blockedModules
      , completedInterfaces = completed
      , documentation = []
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


-- PARALLEL BUILDS!!!

build
    :: BM.Config
    -> Int
    -> Package
    -> Set.Set CanonicalModule
    -> [CanonicalModule]
    -> Map.Map CanonicalModule [CanonicalModule]
    -> BuildGraph
    -> IO [Docs.Documentation]
build config numProcessors rootPkg exposedModules modulesForGeneration dependencies summary =
  do  env <- initEnv numProcessors (BM._artifactDirectory config) exposedModules modulesForGeneration dependencies summary
      forkIO (buildManager env (initState summary))
      Report.thread (BM._reportType config) (BM._warn config) (reportChan env) rootPkg (numTasks env)
      Chan.readChan (docsChan env)


buildManager :: Env -> State -> IO ()
buildManager env state =
  case currentState state of
    _ | numIncompleteTasks state == 0 ->
      do  Chan.writeChan (reportChan env) Report.Close
          Chan.writeChan (docsChan env) (documentation state)

    Wait ->
      do  (Result source path modul threadId dealiaser warnings result) <-
              Chan.readChan (resultChan env)

          if null warnings
            then return ()
            else
              Chan.writeChan (reportChan env)
                  (Report.Warn modul dealiaser path source warnings)

          case result of
            Right (Compiler.Result maybeDocs interface js) ->
              do  let cache = cachePath env
                  File.writeBinary (Path.toInterface cache modul) interface
                  writeFile (Path.toObjectFile cache modul) js
                  Chan.writeChan (reportChan env) (Report.Complete modul)
                  buildManager env (registerSuccess env state modul interface maybeDocs threadId)

            Left errors ->
              do  Chan.writeChan (reportChan env) (Report.Error modul dealiaser path source errors)
                  buildManager env (registerFailure state threadId)

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


-- WAIT - REGISTER RESULTS

registerFailure :: State -> ThreadId -> State
registerFailure state threadId =
  state
    { currentState = Update
    , activeThreads = Set.delete threadId (activeThreads state)
    }


registerSuccess
    :: Env
    -> State
    -> CanonicalModule
    -> Module.Interface
    -> Maybe Docs.Documentation
    -> ThreadId
    -> State
registerSuccess env state name interface maybeDocs threadId =
  let
    (updatedBlockedModules, readyModules) =
      List.mapAccumR
          (updateBlockedModules name)
          (blockedModules state)
          (Maybe.fromMaybe [] (Map.lookup name (reverseDependencies env)))

    newReadyQueue =
      Queue.enqueue (Maybe.catMaybes readyModules) (readyQueue state)

    newCompletedInterfaces =
      Map.insert name interface (completedInterfaces state)
  in
    state
      { currentState = Update
      , activeThreads = Set.delete threadId (activeThreads state)
      , blockedModules = updatedBlockedModules
      , readyQueue = newReadyQueue
      , completedInterfaces = newCompletedInterfaces
      , documentation = maybe id (:) maybeDocs (documentation state)
      }


updateBlockedModules
    :: CanonicalModule
    -> Map.Map CanonicalModule BuildData
    -> CanonicalModule
    -> (Map.Map CanonicalModule BuildData, Maybe (CanonicalModule, Location))
updateBlockedModules modul blockedModules potentiallyFreedModule =
  case Map.lookup potentiallyFreedModule blockedModules of
    Nothing ->
        (blockedModules, Nothing)

    Just (BuildData blocking location) ->
        case filter (/= modul) blocking of
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
    -> Map.Map CanonicalModule Module.Interface
    -> (CanonicalModule, Location)
    -> IO ()
buildModule env interfaces (modul, location) =
  let
    packageName = fst (TMP.package modul)
    path = Path.toSource location
    ifaces = Map.mapKeys simplifyModuleName interfaces
    isRoot = Set.member modul (modulesForGeneration env)
    isExposed = Set.member modul (exposedModules env)

    deps =
        map simplifyModuleName ((Map.!) (dependencies env) modul)

    context =
        Compiler.Context packageName isRoot isExposed deps
  in
  do  source <- readFile path

      let (dealiaser, warnings, rawResult) =
            Compiler.compile context source ifaces

      threadId <- myThreadId
      let result =
            Result source path modul threadId dealiaser warnings rawResult

      Chan.writeChan (resultChan env) result


simplifyModuleName :: TMP.CanonicalModule -> Module.CanonicalName
simplifyModuleName (TMP.CanonicalModule (pkg,_) name) =
    Module.canonicalName pkg name


data Result = Result
    { _source :: String
    , _path :: FilePath
    , _moduleID :: CanonicalModule
    , _threadId :: ThreadId
    , _dealiaser :: Compiler.Dealiaser
    , _warnings :: [Compiler.Warning]
    , _result :: Either [Compiler.Error] Compiler.Result
    }
