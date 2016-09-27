module Pipeline.Compile where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as LazyTextIO
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs

import qualified BuildManager as BM
import qualified Path
import qualified Report
import qualified Utils.File as File
import qualified TheMasterPlan as TMP
import TheMasterPlan
    ( CanonicalModule(..), Location, Package
    , BuildGraph(BuildGraph), BuildData(..)
    )



-- ENVIRONMENT and STATE


data Env =
  Env
    { numTasks :: Int
    , resultChan :: Chan.Chan Result
    , reportChan :: Chan.Chan Report.Message
    , doneChan :: Chan.Chan (Interfaces, [Docs.Documentation])
    , dependencies :: Map.Map CanonicalModule [CanonicalModule]
    , reverseDependencies :: Map.Map CanonicalModule [CanonicalModule]
    , cachePath :: FilePath
    , exposedModules :: Set.Set CanonicalModule
    , modulesForGeneration :: Set.Set CanonicalModule
    }


data State =
  State
    { numActiveThreads :: Int
    , blockedModules :: Map.Map CanonicalModule BuildData
    , completedInterfaces :: Interfaces
    , documentation :: [Docs.Documentation]
    }


type Interfaces =
  Map.Map CanonicalModule Module.Interface



-- HELPERS for ENV and STATE


initEnv
    :: FilePath
    -> Set.Set CanonicalModule
    -> [CanonicalModule]
    -> Map.Map CanonicalModule [CanonicalModule]
    -> BuildGraph
    -> IO Env
initEnv cachePath exposedModules modulesForGeneration dependencies (BuildGraph blocked _completed) =
  do  resultChan <- Chan.newChan
      reportChan <- Chan.newChan
      doneChan <- Chan.newChan
      return $ Env
        { numTasks = Map.size blocked
        , resultChan = resultChan
        , reportChan = reportChan
        , doneChan = doneChan
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


initState :: Env -> BuildGraph -> IO State
initState env (BuildGraph blocked completed) =
  let
    categorize name buildData@(BuildData blocking location) =
      case blocking of
        [] ->
          Left (name, location)

        _  ->
          Right buildData

    (readyModules, blockedModules) =
      Map.mapEitherWithKey categorize blocked

    readyList =
      Map.elems readyModules
  in
    do  mapM (forkIO . buildModule env completed) readyList
        return $
          State
            { numActiveThreads = length readyList
            , blockedModules = blockedModules
            , completedInterfaces = completed
            , documentation = []
            }



-- PARALLEL BUILDS!!!


build
    :: BM.Config
    -> Package
    -> Set.Set CanonicalModule
    -> [CanonicalModule]
    -> Map.Map CanonicalModule [CanonicalModule]
    -> BuildGraph
    -> IO (Interfaces, [Docs.Documentation])
build config rootPkg exposedModules modulesForGeneration dependencies summary =
  do  env <- initEnv (BM._artifactDirectory config) exposedModules modulesForGeneration dependencies summary
      forkIO (buildManager env =<< initState env summary)
      Report.thread (BM._reportType config) (BM._warn config) (reportChan env) rootPkg (numTasks env)
      Chan.readChan (doneChan env)


buildManager :: Env -> State -> IO ()
buildManager env state =
  if numActiveThreads state == 0 then

    do  Chan.writeChan (reportChan env) Report.Close
        Chan.writeChan (doneChan env) (completedInterfaces state, documentation state)

  else

    do  (Result source path modul localizer warnings result) <-
          Chan.readChan (resultChan env)

        case result of
          Right (Compiler.Result maybeDocs interface js) ->
            do  -- Write build artifacts to disk
                let cache = cachePath env
                File.writeBinary (Path.toInterface cache modul) interface
                LazyTextIO.writeFile (Path.toObjectFile cache modul) js

                -- Report results to user
                Chan.writeChan (reportChan env) (Report.Complete modul localizer path source warnings)

                -- Loop
                newState <- registerSuccess env state modul interface maybeDocs
                buildManager env newState

          Left errors ->
            do  -- Report results to user
                Chan.writeChan (reportChan env) (Report.Error modul localizer path source warnings errors)

                -- Loop
                buildManager env (state { numActiveThreads = numActiveThreads state - 1 })



-- WAIT - REGISTER RESULTS


registerSuccess
    :: Env
    -> State
    -> CanonicalModule
    -> Module.Interface
    -> Maybe Docs.Documentation
    -> IO State
registerSuccess env state name interface maybeDocs =
  let
    (updatedBlockedModules, readyModules) =
      List.mapAccumR
        (updateBlockedModules name)
        (blockedModules state)
        (Maybe.fromMaybe [] (Map.lookup name (reverseDependencies env)))

    readyList =
      Maybe.catMaybes readyModules

    newCompletedInterfaces =
      Map.insert name interface (completedInterfaces state)
  in
    do  mapM (forkIO . buildModule env newCompletedInterfaces) readyList

        return $
          state
            { numActiveThreads = numActiveThreads state - 1 + length readyList
            , blockedModules = updatedBlockedModules
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


buildModule :: Env -> Interfaces -> (CanonicalModule, Location) -> IO ()
buildModule env interfaces (modul, location) =
  let
    packageName = fst (TMP.package modul)
    path = Path.toSource location
    ifaces = Map.mapKeys TMP.simplifyModuleName interfaces
    isExposed = Set.member modul (exposedModules env)

    deps =
        map TMP.simplifyModuleName ((Map.!) (dependencies env) modul)

    context =
        Compiler.Context packageName isExposed deps
  in
  do  source <- readFile path

      let (localizer, warnings, rawResult) =
            Compiler.compile context source ifaces

      let result =
            Result source path modul localizer warnings rawResult

      Chan.writeChan (resultChan env) result


data Result = Result
    { _source :: String
    , _path :: FilePath
    , _moduleID :: CanonicalModule
    , _localizer :: Compiler.Localizer
    , _warnings :: [Compiler.Warning]
    , _result :: Either [Compiler.Error] Compiler.Result
    }
