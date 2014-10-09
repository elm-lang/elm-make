{-# LANGUAGE FlexibleContexts #-}
module Crawl where

import Control.Arrow (second)
import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Crawl.Local as Local
import qualified Crawl.Locations as Locations
import qualified Crawl.Packages as Pkg
import qualified Crawl.Validate as Validate
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V


type Graph =
    Map.Map Module.Name [Module.Name]


crawl :: (MonadIO m, MonadError String m) => m (Locations.Locations, Graph)
crawl =
  do  description <- Desc.read Path.description

      let sourceDirs = Desc.sourceDirs description
      let exposedModules = Desc.exposed description

      locals <- mapM (Local.findIn sourceDirs) exposedModules

      initialLocations <- Locations.initialize (zip exposedModules locals)

      (locations, dependencyNodes) <-
          depthFirstSearch sourceDirs initialLocations Map.empty exposedModules

      checkForCycles dependencyNodes

      return (locations, dependencyNodes)


-- DEPTH FIRST SEARCH

depthFirstSearch
    :: (MonadIO m, MonadError String m)
    => [FilePath]
    -> Locations.Locations
    -> Graph
    -> [Module.Name]
    -> m (Locations.Locations, Graph)

depthFirstSearch _sourceDirs locations dependencyNodes [] =
    return (locations, dependencyNodes)

depthFirstSearch sourceDirs locations dependencyNodes (moduleName:unvisited) =
  case Map.lookup moduleName locations of
    Just (Locations.Package _name) ->
        depthFirstSearch sourceDirs locations dependencyNodes unvisited

    Just (Locations.Local path) ->
        do  source <- liftIO (readFile path)
            readFileAndContinue path locations

    Nothing ->
        do  path <- Local.findIn sourceDirs moduleName
            let updatedLocations =
                    Map.insert moduleName (Locations.Local path) locations
            readFileAndContinue path updatedLocations

  where
    readFileAndContinue path updatedLocations =
        do  source <- liftIO (readFile path)
            (_, deps) <- Compiler.parseDependencies source

            let newUnvisited =
                    filter (\name -> not (Map.member name dependencyNodes)) deps

            depthFirstSearch
                sourceDirs
                updatedLocations
                (Map.insert moduleName deps dependencyNodes)
                (unvisited ++ newUnvisited)


-- CHECK FOR CYCLES

checkForCycles :: (MonadError String m) => Graph -> m ()
checkForCycles dependencyNodes =
    mapM_ errorOnCycle components
  where
    components =
        Graph.stronglyConnComp (map toNode (Map.toList dependencyNodes))

    toNode (name, deps) =
        (name, name, deps)

    errorOnCycle scc =
        case scc of
          Graph.AcyclicSCC _ -> return ()
          Graph.CyclicSCC cycle ->
              throwError $
              "Your dependencies for a cycle:\n\n"
              ++ showCycle dependencyNodes cycle
              ++ "\nYou may need to move some values to a new module to get rid of thi cycle."


showCycle :: Graph -> [Module.Name] -> String
showCycle _dependencyNodes [] = ""
showCycle dependencyNodes (name:rest) =
    "    " ++ Module.nameToString name ++ " => " ++ Module.nameToString next ++ "\n"
    ++ showCycle dependencyNodes (next:remaining)
  where
    ([next], remaining) =
        List.partition (`elem` rest) (dependencyNodes Map.! name)
