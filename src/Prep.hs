{-# LANGUAGE FlexibleContexts #-}
module Prep where

import Control.Monad.Error (MonadError, throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import System.Directory (doesFileExist, getModificationTime)
import qualified Elm.Compiler.Module as Module


--- LOAD INTERFACES -- what has already been compiled?

loadInterfaces
    :: (Module.Name -> FilePath)
    -> Map.Map Module.Name FilePath
    -> IO (Map.Map Module.Name (FilePath, Maybe Module.Interface))
loadInterfaces interfacePath locatedModules =
    Traversable.traverse id $
        Map.mapWithKey (loadInterface interfacePath) locatedModules


loadInterface
    :: (Module.Name -> FilePath)
    -> Module.Name
    -> FilePath
    -> IO (FilePath, Maybe Module.Interface)
loadInterface interfacePath name path =
  do  let elmi = interfacePath name
      fresh <- isFresh elmi path
      case fresh of
        False -> return (path, Nothing)
        True ->
          do  interface <- (error "Module.readInterface") elmi
              return (path, Just interface)
                    

isFresh :: FilePath -> FilePath -> IO Bool
isFresh elmi path =
  do  exists <- doesFileExist elmi
      case exists of
        False -> return False
        True ->
          do  pathTime <- getModificationTime path
              elmiTime <- getModificationTime elmi
              return (pathTime <= elmiTime)


-- FILTER STALE INTERFACES -- have files become stale due to other changes?

filterStaleInterfaces
    :: (MonadError String m)
    => Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (FilePath, Maybe Module.Interface)
    -> m (Map.Map Module.Name (FilePath, Maybe Module.Interface))
filterStaleInterfaces dependencies locatedModules =
  do  sortedNames <- topologicalSort dependencies
      return $ List.foldl' (filterIfStale dependencies) locatedModules sortedNames


filterIfStale
    :: Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (FilePath, Maybe Module.Interface)
    -> Module.Name
    -> Map.Map Module.Name (FilePath, Maybe Module.Interface)
filterIfStale dependencies locatedModules name
    | noInterface locatedModules name =
        locatedModules
    | any (noInterface locatedModules) (dependencies ! name) =
        Map.adjust (\(fp,_) -> (fp, Nothing)) name locatedModules
    | otherwise =
        locatedModules


noInterface
    :: Map.Map Module.Name (FilePath, Maybe Module.Interface)
    -> Module.Name
    -> Bool
noInterface locatedModules name =
    case locatedModules ! name of
      (_, Just _) -> False
      (_, Nothing) -> True


-- ENRICH DEPENDENCIES -- augment dependencies based on available interfaces

enrichDependencies
    :: Map.Map Module.Name [Module.Name]
    -> Map.Map Module.Name (FilePath, Maybe Module.Interface)
    -> Map.Map Module.Name ([Module.Name], Map.Map Module.Name Module.Interface)
enrichDependencies dependencies locatedModules =
    Map.map (enrich interfaces) dependencies
  where
    interfaces =
        Map.mapMaybe snd locatedModules

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

graphReverse :: (Ord a) => Map.Map a [a] -> Map.Map a [a]
graphReverse graph =
    Map.foldrWithKey flipEdges Map.empty graph
  where
    flipEdges name dependencies reversedGraph =
        foldr (insertDependency name) reversedGraph dependencies

    insertDependency name dep reversedGraph =
        Map.insertWith (++) dep [name] reversedGraph
