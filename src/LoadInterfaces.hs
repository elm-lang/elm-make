{-# LANGUAGE FlexibleContexts #-}
module LoadInterfaces where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import Control.Monad.Reader (MonadReader)
import qualified Data.Graph as Graph
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory (doesFileExist, getModificationTime)

import qualified Elm.Compiler.Module as Module
import qualified Path
import TheMasterPlan
    ( ModuleID(ModuleID), Location(..)
    , ProjectSummary(ProjectSummary), ProjectData(..)
    , BuildSummary(..), BuildData(..)
    )


prepForBuild
    :: (MonadIO m, MonadError String m, MonadReader FilePath m)
    => ProjectSummary Location
    -> m BuildSummary
prepForBuild (ProjectSummary projectData _projectNatives) =
  do  enhancedData <- addInterfaces projectData
      filteredData <- filterStaleInterfaces enhancedData
      return (toBuildSummary filteredData)


--- LOAD INTERFACES -- what has already been compiled?

addInterfaces
    :: (MonadIO m, MonadReader FilePath m)
    => Map.Map ModuleID (ProjectData Location)
    -> m (Map.Map ModuleID (ProjectData (Location, Maybe Module.Interface)))
addInterfaces projectData =
  do  enhancedData <- mapM maybeLoadInterface (Map.toList projectData)
      return (Map.fromList enhancedData)
      

maybeLoadInterface
    :: (MonadIO m, MonadReader FilePath m)
    => (ModuleID, ProjectData Location)
    -> m (ModuleID, ProjectData (Location, Maybe Module.Interface))
maybeLoadInterface (moduleID, (ProjectData location deps)) =
  do  interfacePath <- Path.fromModuleID moduleID
      let sourcePath = Path.fromLocation location
      fresh <- liftIO (isFresh sourcePath interfacePath)

      maybeInterface <-
          case fresh of
            False -> return Nothing
            True ->
              do  interface <- (error "Module.readInterface") interfacePath
                  return (Just interface)

      return (moduleID, ProjectData (location, maybeInterface) deps)
                    

isFresh :: FilePath -> FilePath -> IO Bool
isFresh sourcePath interfacePath =
  do  exists <- doesFileExist interfacePath
      case exists of
        False -> return False
        True ->
          do  sourceTime <- getModificationTime sourcePath
              interfaceTime <- getModificationTime interfacePath
              return (sourceTime <= interfaceTime)


-- FILTER STALE INTERFACES -- have files become stale due to other changes?

filterStaleInterfaces
    :: (MonadError String m)
    => Map.Map ModuleID (ProjectData (Location, Maybe Module.Interface))
    -> m (Map.Map ModuleID (ProjectData (Either Location Module.Interface)))
filterStaleInterfaces summary =
  do  sortedNames <- topologicalSort (Map.map projectDependencies summary)
      return (List.foldl' (filterIfStale summary) Map.empty sortedNames)


filterIfStale
    :: Map.Map ModuleID (ProjectData (Location, Maybe Module.Interface))
    -> Map.Map ModuleID (ProjectData (Either Location Module.Interface))
    -> ModuleID
    -> Map.Map ModuleID (ProjectData (Either Location Module.Interface))
filterIfStale enhancedSummary filteredSummary moduleName =
    Map.insert moduleName (ProjectData trueLocation deps) filteredSummary
  where
    (ProjectData (filePath, maybeInterface) deps) =
        enhancedSummary ! moduleName

    trueLocation =
        case maybeInterface of
          Just interface
            | all (haveInterface enhancedSummary) deps ->
                Right interface

          _ -> Left filePath


haveInterface
    :: Map.Map ModuleID (ProjectData (Location, Maybe Module.Interface))
    -> ModuleID
    -> Bool
haveInterface enhancedSummary name =
    case projectLocation (enhancedSummary ! name) of
      (_, Just _) -> True
      (_, Nothing) -> False


-- FILTER DEPENDENCIES -- which modules actually need to be compiled?

toBuildSummary
    :: Map.Map ModuleID (ProjectData (Either Location Module.Interface))
    -> BuildSummary
toBuildSummary summary =
    BuildSummary
    { blockedModules = Map.map (toBuildData interfaces) locations
    , completedInterfaces = interfaces
    }
  where
    (locations, interfaces) =
        Map.mapEither divide summary

    divide (ProjectData either deps) =
        case either of
          Left location ->
              Left (ProjectData location deps)

          Right interface ->
              Right interface

toBuildData
    :: Map.Map ModuleID Module.Interface
    -> ProjectData Location
    -> BuildData
toBuildData interfaces (ProjectData location dependencies) =
    BuildData blocking location
  where
    blocking =
        Maybe.mapMaybe filterDeps dependencies

    filterDeps :: ModuleID -> Maybe ModuleID
    filterDeps deps =
        filterCachedDeps interfaces =<< filterNativeDeps deps


filterCachedDeps
    :: Map.Map ModuleID Module.Interface
    -> ModuleID
    -> Maybe ModuleID
filterCachedDeps interfaces name =
    case Map.lookup name interfaces of
      Just _interface -> Nothing
      Nothing -> Just name


filterNativeDeps :: ModuleID -> Maybe ModuleID
filterNativeDeps name =
    case name of
      ModuleID (Module.Name ("Native" : _)) _pkg ->
          Nothing

      _ ->
          Just name


-- SORT GRAPHS / CHECK FOR CYCLES

topologicalSort :: (MonadError String m) => Map.Map ModuleID [ModuleID] -> m [ModuleID]
topologicalSort dependencies =
    mapM errorOnCycle components
  where
    components =
        Graph.stronglyConnComp (map toNode (Map.toList dependencies))

    toNode (name, deps) =
        (name, name, deps)

    errorOnCycle scc =
        case scc of
          Graph.AcyclicSCC name -> return name
          Graph.CyclicSCC cycle ->
              throwError $
              "Your dependencies for a cycle:\n\n"
              ++ showCycle dependencies cycle
              ++ "\nYou may need to move some values to a new module to get rid of thi cycle."


showCycle :: Map.Map ModuleID [ModuleID] -> [ModuleID] -> String
showCycle _dependencies [] = ""
showCycle dependencies (name:rest) =
    "    " ++ idToString name ++ " => " ++ idToString next ++ "\n"
    ++ showCycle dependencies (next:remaining)
  where
    idToString (ModuleID moduleName _pkg) =
        Module.nameToString moduleName
    ([next], remaining) =
        List.partition (`elem` rest) (dependencies Map.! name)

