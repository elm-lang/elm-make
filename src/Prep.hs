{-# LANGUAGE FlexibleContexts #-}
module Prep where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import System.Directory (doesFileExist, getModificationTime)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import qualified Path


{-| A well-structured summary of info gained by crawling a project.

  * locations
      If a module needs to be compiled, you get the file path.
      If already compiled and unimpacted by changes, you get the interface.

  * dependencies
      Each module is associated with its dependencies. If the dependency is
      not ready yet, it goes in the list of names. If the dependency is ready,
      it appears in the Map with the corresponding interface. When the list is
      empty, it indicates that it is safe to build that module!

-}
data Info = Info
    { locations :: Map.Map Module.Name (Either LocationData Module.Interface)
    , dependencies :: Map.Map Module.Name ([Module.Name], Map.Map Module.Name Module.Interface)
    }

data LocationData = LocationData
    { package :: Maybe Pkg.Name
    , sourcePath :: FilePath
    }

prep
    :: (MonadIO m, MonadError String m)
    => Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name LocationData
    -> m Info
prep dependencies locatedModules =
  do  possibleLocations <- loadInterfaces locatedModules
      trueLocations <- filterStaleInterfaces dependencies possibleLocations
      let enrichedDependencies = enrichDependencies dependencies trueLocations
      return $ Info {
          locations = trueLocations,
          dependencies = enrichedDependencies
      }


--- LOAD INTERFACES -- what has already been compiled?

loadInterfaces
    :: (MonadIO m)
    => Map.Map Module.Name LocationData
    -> m (Map.Map Module.Name (LocationData, Maybe Module.Interface))
loadInterfaces locatedModules =
  do  maybeInterfaces <- mapM maybeLoadInterface locations
      return (Map.fromList (zipWith combine locations maybeInterfaces))
  where
    locations = Map.toList locatedModules
    combine (moduleName, locationData) maybeInterface =
        (moduleName, (locationData, maybeInterface))
      

maybeLoadInterface :: (MonadIO m)
    => (Module.Name, LocationData)
    -> m (Maybe Module.Interface)
maybeLoadInterface (moduleName, LocationData pkgName sourcePath) =
  do  let interfacePath = Path.toInterface pkgName moduleName
      fresh <- liftIO (isFresh sourcePath interfacePath)
      case fresh of
        False -> return Nothing
        True ->
          do  interface <- (error "Module.readInterface") interfacePath
              return (Just interface)
                    

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
    => Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (LocationData, Maybe Module.Interface)
    -> m (Map.Map Module.Name (Either LocationData Module.Interface))
filterStaleInterfaces dependencies possibleLocations =
  do  sortedNames <- topologicalSort dependencies
      return $ List.foldl' insert Map.empty sortedNames
  where
    insert trueLocations moduleName =
        filterIfStale dependencies possibleLocations moduleName trueLocations


filterIfStale
    :: Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (LocationData, Maybe Module.Interface)
    -> Module.Name
    -> Map.Map Module.Name (Either LocationData Module.Interface)
    -> Map.Map Module.Name (Either LocationData Module.Interface)
filterIfStale dependencies possibleLocations moduleName trueLocations =
    case possibleLocations ! moduleName of
      (_, Just interface)
          | all (haveInterface possibleLocations) (dependencies ! moduleName) ->
              Map.insert moduleName (Right interface) trueLocations

      (filePath, _) ->
          Map.insert moduleName (Left filePath) trueLocations


haveInterface
    :: Map.Map Module.Name (LocationData, Maybe Module.Interface)
    -> Module.Name
    -> Bool
haveInterface possibleLocations name =
    case possibleLocations ! name of
      (_, Just _) -> True
      (_, Nothing) -> False


-- ENRICH DEPENDENCIES -- augment dependencies based on available interfaces

enrichDependencies
    :: Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (Either LocationData Module.Interface)
    -> Map.Map Module.Name ([Module.Name], Map.Map Module.Name Module.Interface)
enrichDependencies dependencies trueLocations =
    Map.map (enrich interfaces) dependencies
  where
    interfaces =
        Map.mapMaybe (either (const Nothing) Just) trueLocations

enrich
    :: Map.Map Module.Name Module.Interface
    -> [Module.Name]
    -> ([Module.Name], Map.Map Module.Name Module.Interface)
enrich interfaces dependencies =
    List.foldl' insert ([], Map.empty) dependencies
  where
    insert (blocking, ready) name =
        case Map.lookup name interfaces of
          Just interface ->
              (blocking, Map.insert name interface ready)
          Nothing ->
              (name : blocking, ready)


-- SORT GRAPHS / CHECK FOR CYCLES

topologicalSort :: (MonadError String m) => Map.Map Module.Name [Module.Name] -> m [Module.Name]
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


showCycle :: Map.Map Module.Name [Module.Name] -> [Module.Name] -> String
showCycle _dependencies [] = ""
showCycle dependencies (name:rest) =
    "    " ++ Module.nameToString name ++ " => " ++ Module.nameToString next ++ "\n"
    ++ showCycle dependencies (next:remaining)
  where
    ([next], remaining) =
        List.partition (`elem` rest) (dependencies Map.! name)


-- REVERSE GRAPHS -- help us reverse dependencies, "who depends on me?"

reverseGraph :: (Ord a) => Map.Map a [a] -> Map.Map a [a]
reverseGraph graph =
    Map.foldrWithKey flipEdges Map.empty graph
  where
    flipEdges name dependencies reversedGraph =
        foldr (insertDependency name) reversedGraph dependencies

    insertDependency name dep reversedGraph =
        Map.insertWith (++) dep [name] reversedGraph
