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
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution


crawlEverything :: (MonadIO m, MonadError String m) => m ()
crawlEverything =
  do  solution <- Solution.read Path.solvedDependencies

      packageInfo <-
          forM (Map.toList solution) $ \(name,version) -> do
              state <- Dfs.crawl (Path.package name version) solution Nothing
              return (name, state)

      _ <- Dfs.crawl "." solution Nothing
      return ()


crawlFile :: (MonadIO m, MonadError String m) => FilePath -> m Dfs.State
crawlFile path =
  do  solution <- Solution.read Path.solvedDependencies
      Dfs.crawl "." solution (Just path)

