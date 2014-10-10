{-# LANGUAGE FlexibleContexts #-}
module Crawl where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Crawl.DepthFirstSearch as Dfs
import qualified Crawl.Packages as Package
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution


crawlEverything :: (MonadIO m, MonadError String m) => m ()
crawlEverything =
  do  solution <- Solution.read Path.solvedDependencies

      packageInfo <-
          forM (Map.toList solution) $ \(name,version) -> do
              state <- Crawl.crawl (Path.package name version) solution Nothing
              return (name, state)

      _ <- Crawl.crawl "." solution Nothing
      return ()


crawlFile :: (MonadIO m, MonadError String m) => FilePath -> m Dfs.State
crawlFile path =
  do  solution <- Solution.read Path.solvedDependencies
      crawl "." solution (Just path)


crawl :: (MonadIO m, MonadError String m) => FilePath -> Solution.Solution -> Maybe FilePath -> m Dfs.State
crawl root solution maybeFilePath =
  do  desc <- Desc.read (root </> Path.description)

      exposedModules <- Package.allExposedModules desc solution
      let sourceDirs = map (root </>) (Desc.sourceDirs desc)
      let env = Dfs.Env sourceDirs exposedModules

      state <-
          case maybeFilePath of
            Just path ->
                Dfs.crawlFile path Nothing [] env Dfs.initialState
            Nothing ->
                Dfs.crawlDependencies (Desc.exposed desc) env Dfs.initialState

      checkForCycles (Dfs.dependencies state)

      return state


-- CHECK FOR CYCLES

checkForCycles :: (MonadError String m) => Map.Map Module.Name [Module.Name] -> m ()
checkForCycles dependencies =
    mapM_ errorOnCycle components
  where
    components =
        Graph.stronglyConnComp (map toNode (Map.toList dependencies))

    toNode (name, deps) =
        (name, name, deps)

    errorOnCycle scc =
        case scc of
          Graph.AcyclicSCC _ -> return ()
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
