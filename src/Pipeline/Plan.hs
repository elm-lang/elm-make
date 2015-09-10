module Pipeline.Plan where

import Control.Monad.Except (liftIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Elm.Compiler.Module as Module
import System.Directory (doesFileExist, getModificationTime)

import qualified BuildManager as BM
import qualified Path
import qualified Utils.File as File
import TheMasterPlan
    ( CanonicalModule(CanonicalModule), Location(..)
    , ProjectGraph(ProjectGraph), ProjectData(..)
    , BuildGraph(..), BuildData(..)
    )


planBuild :: BM.Config -> Set.Set CanonicalModule -> ProjectGraph Location -> BM.Task BuildGraph
planBuild config modulesToDocument (ProjectGraph projectData _projectNatives) =
  do  enhancedData <- addInterfaces (BM._artifactDirectory config) projectData
      filteredData <- filterStaleInterfaces modulesToDocument enhancedData
      return (toBuildGraph filteredData)


--- LOAD INTERFACES -- what has already been compiled?

addInterfaces
    :: FilePath
    -> Map.Map CanonicalModule (ProjectData Location)
    -> BM.Task (Map.Map CanonicalModule (ProjectData (Location, Maybe Module.Interface)))
addInterfaces artifactRoot projectData =
  do  enhancedData <- mapM (maybeLoadInterface artifactRoot) (Map.toList projectData)
      return (Map.fromList enhancedData)


-- TODO: if two modules in the same package have the same name, their interface
-- files will be indistinguishable right now. The most common case of this is
-- modules named Main. As a stopgap, we never load in the interface file for
-- Main. The real fix may be to add a hash of the source code to the interface
-- files.
maybeLoadInterface
    :: FilePath
    -> (CanonicalModule, ProjectData Location)
    -> BM.Task (CanonicalModule, ProjectData (Location, Maybe Module.Interface))
maybeLoadInterface artifactRoot (moduleID, (ProjectData location deps)) =
  do  let interfacePath = Path.toInterface artifactRoot moduleID
      let sourcePath = Path.toSource location
      fresh <- liftIO (isFresh sourcePath interfacePath)

      maybeInterface <-
          case fresh && not (isMain moduleID) of
            False -> return Nothing
            True ->
              do  interface <- File.readBinary interfacePath
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


isMain :: CanonicalModule -> Bool
isMain (CanonicalModule _ (Module.Name names)) =
    case names of
      ["Main"] -> True
      _ -> False


-- FILTER STALE INTERFACES -- have files become stale due to other changes?

filterStaleInterfaces
    :: Set.Set CanonicalModule
    -> Map.Map CanonicalModule (ProjectData (Location, Maybe Module.Interface))
    -> BM.Task (Map.Map CanonicalModule (ProjectData (Either Location Module.Interface)))
filterStaleInterfaces modulesToDocument summary =
  do  sortedNames <- topologicalSort (Map.map projectDependencies summary)
      return (List.foldl' (filterIfStale summary modulesToDocument) Map.empty sortedNames)


filterIfStale
    :: Map.Map CanonicalModule (ProjectData (Location, Maybe Module.Interface))
    -> Set.Set CanonicalModule
    -> Map.Map CanonicalModule (ProjectData (Either Location Module.Interface))
    -> CanonicalModule
    -> Map.Map CanonicalModule (ProjectData (Either Location Module.Interface))
filterIfStale enhancedGraph modulesToDocument filteredGraph moduleName =
    Map.insert moduleName (ProjectData trueLocation deps) filteredGraph
  where
    (ProjectData (filePath, maybeInterface) deps) =
        enhancedGraph ! moduleName

    depsAreDone =
      all (haveInterface filteredGraph) deps

    needsDocs =
      Set.member moduleName modulesToDocument

    trueLocation =
        case maybeInterface of
          Just interface
            | depsAreDone && not needsDocs ->
                Right interface

          _ -> Left filePath


haveInterface
    :: Map.Map CanonicalModule (ProjectData (Either Location Module.Interface))
    -> CanonicalModule
    -> Bool
haveInterface enhancedGraph rawName =
    case filterNativeDeps rawName of
      Nothing -> True
      Just name ->
          case Map.lookup name enhancedGraph of
            Just (ProjectData (Right _) _) -> True
            _ -> False


-- FILTER DEPENDENCIES -- which modules actually need to be compiled?

toBuildGraph
    :: Map.Map CanonicalModule (ProjectData (Either Location Module.Interface))
    -> BuildGraph
toBuildGraph summary =
    BuildGraph
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
    :: Map.Map CanonicalModule Module.Interface
    -> ProjectData Location
    -> BuildData
toBuildData interfaces (ProjectData location dependencies) =
    BuildData blocking location
  where
    blocking =
        Maybe.mapMaybe filterDeps dependencies

    filterDeps :: CanonicalModule -> Maybe CanonicalModule
    filterDeps deps =
        filterCachedDeps interfaces =<< filterNativeDeps deps


filterCachedDeps
    :: Map.Map CanonicalModule Module.Interface
    -> CanonicalModule
    -> Maybe CanonicalModule
filterCachedDeps interfaces name =
    case Map.lookup name interfaces of
      Just _interface -> Nothing
      Nothing -> Just name


filterNativeDeps :: CanonicalModule -> Maybe CanonicalModule
filterNativeDeps name =
    case name of
      CanonicalModule _pkg (Module.Name ("Native" : _)) ->
          Nothing

      _ ->
          Just name


-- SORT GRAPHS / CHECK FOR CYCLES

topologicalSort :: Map.Map CanonicalModule [CanonicalModule] -> BM.Task [CanonicalModule]
topologicalSort dependencies =
    mapM errorOnCycle components
  where
    components =
        Graph.stronglyConnComp (map toNode (Map.toList dependencies))

    toNode (name, deps) =
        (name, name, deps)

    errorOnCycle scc =
        case scc of
          Graph.AcyclicSCC name ->
              return name

          Graph.CyclicSCC cycle ->
              throwError (BM.Cycle cycle)
