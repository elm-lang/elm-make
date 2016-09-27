module Pipeline.Plan where

import Control.Monad (foldM)
import Control.Monad.Except (liftIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
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
  do  enhancedData <- Map.traverseWithKey (enhanceData (BM._artifactDirectory config)) projectData
      filteredData <- loadCachedInterfaces modulesToDocument enhancedData
      return (toBuildGraph filteredData)


type EnhancedGraph =
  Map.Map CanonicalModule (ProjectData (Location, Maybe (FilePath, UTCTime)))


type InterfacedGraph =
  Map.Map CanonicalModule (ProjectData (Either Location Module.Interface))



--- LOAD INTERFACES -- what has already been compiled?


-- TODO: if two modules in the same package have the same name, their interface
-- files will be indistinguishable right now. The most common case of this is
-- modules named Main. As a stopgap, we never load in the interface file for
-- Main. The real fix may be to add a hash of the source code to the interface
-- files.
enhanceData
    :: FilePath
    -> CanonicalModule
    -> ProjectData Location
    -> BM.Task (ProjectData (Location, Maybe (FilePath, UTCTime)))
enhanceData artifactRoot moduleID (ProjectData location deps) =
  if isMain moduleID then
    return $ ProjectData (location, Nothing) deps

  else
    do  let interfacePath = Path.toInterface artifactRoot moduleID
        let sourcePath = Path.toSource location
        interfaceInfo <- liftIO $ getFreshInterfaceInfo sourcePath interfacePath
        return $ ProjectData (location, interfaceInfo) deps


getFreshInterfaceInfo :: FilePath -> FilePath -> IO (Maybe (FilePath, UTCTime))
getFreshInterfaceInfo sourcePath interfacePath =
  do  exists <- doesFileExist interfacePath
      case exists of
        False ->
          return Nothing

        True ->
          do  sourceTime <- getModificationTime sourcePath
              interfaceTime <- getModificationTime interfacePath
              return $
                if sourceTime <= interfaceTime then
                  Just (interfacePath, interfaceTime)
                else
                  Nothing


isMain :: CanonicalModule -> Bool
isMain (CanonicalModule _ names) =
  names == ["Main"]



-- FILTER STALE INTERFACES -- have files become stale due to other changes?


loadCachedInterfaces :: Set.Set CanonicalModule -> EnhancedGraph -> BM.Task InterfacedGraph
loadCachedInterfaces modulesToDocument summary =
  do  sortedNames <- topologicalSort (Map.map projectDependencies summary)
      foldM (updateFromCache summary modulesToDocument) Map.empty sortedNames


updateFromCache
    :: EnhancedGraph
    -> Set.Set CanonicalModule
    -> InterfacedGraph
    -> CanonicalModule
    -> BM.Task InterfacedGraph
updateFromCache enhancedGraph modulesToDocument interfacedGraph moduleName =
  do  trueLocation <- getTrueLocation
      return $ Map.insert moduleName (ProjectData trueLocation deps) interfacedGraph
  where
    (ProjectData (location, maybeInterfaceInfo) deps) =
      enhancedGraph ! moduleName

    getTrueLocation =
      case maybeInterfaceInfo of
        Nothing ->
          return $ Left location

        Just (interfacePath, time) ->
          let
            depsAreCached = all (isCacheValid time enhancedGraph interfacedGraph) deps
            needsDocs = Set.member moduleName modulesToDocument
          in
            if depsAreCached && not needsDocs then
              Right <$> File.readBinary interfacePath
            else
              return $ Left location


isCacheValid :: UTCTime -> EnhancedGraph -> InterfacedGraph -> CanonicalModule -> Bool
isCacheValid time enhancedGraph interfacedGraph possiblyNativeName =
  case filterNativeDeps possiblyNativeName of
    Nothing ->
      True

    Just name ->
      let
        getLoc (ProjectData loc _) =
          loc
      in
        case ( getLoc (interfacedGraph ! name), getLoc (enhancedGraph ! name) ) of
          ( Right _, (_, Just (_, depTime)) ) ->
            depTime <= time

          _ ->
            False



-- FILTER DEPENDENCIES -- which modules actually need to be compiled?


toBuildGraph
    :: InterfacedGraph
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
      CanonicalModule _pkg ("Native" : _) ->
          Nothing

      _ ->
          Just name



-- SORT GRAPHS / CHECK FOR CYCLES


topologicalSort :: Map.Map CanonicalModule [CanonicalModule] -> BM.Task [CanonicalModule]
topologicalSort dependencies =
  let
    toNode (name, deps) =
      (name, name, deps)

    components =
      Graph.stronglyConnComp (map toNode (Map.toList dependencies))
  in
    mapM errorOnCycle components


errorOnCycle :: Graph.SCC CanonicalModule -> BM.Task CanonicalModule
errorOnCycle scc =
  case scc of
    Graph.AcyclicSCC name ->
      return name

    Graph.CyclicSCC cycle ->
      throwError (BM.Cycle cycle)
