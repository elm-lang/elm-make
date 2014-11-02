{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, runErrorT, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified Build
import qualified CrawlPackage
import qualified CrawlProject
import qualified LoadInterfaces
import qualified Options
import qualified Elm.Package.Initialize as Initialize
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Generate
import qualified Path as BuildPath
import TheMasterPlan
    ( ModuleID(ModuleID), Location
    , ProjectSummary(..), ProjectData(..)
    )


main :: IO ()
main =
  do  files <- Options.parse

      result <- runErrorT (runReaderT (run files) "cache")
      case result of
        Right () -> return ()
        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure


run :: (MonadIO m, MonadError String m, MonadReader FilePath m)
    => [FilePath]
    -> m ()
run files =
  do  numProcessors <- liftIO getNumProcessors
      liftIO (setNumCapabilities numProcessors)

      (moduleNames, projectSummary) <- crawl files
      let dependencies = Map.map projectDependencies (projectData projectSummary)
      buildSummary <- LoadInterfaces.prepForBuild projectSummary

      cachePath <- ask
      liftIO (Build.build numProcessors cachePath dependencies buildSummary)

      liftIO (Generate.js cachePath dependencies (projectNatives projectSummary) moduleNames "elm.js")


crawl
    :: (MonadIO m, MonadError String m)
    => [FilePath]
    -> m ([ModuleID], ProjectSummary Location)
crawl filePaths =
  do  solution <- getSolution

      summaries <-
          forM (Map.toList solution) $ \(name,version) -> do
              let root = BuildPath.fromPackage name version
              packageSummary <- CrawlPackage.dfsFromExposedModules root solution
              return (CrawlProject.canonicalizePackageSummary (Just (name,version)) packageSummary)

      (moduleNames, packageSummary) <-
          case filePaths of
            [] ->
              do  summary <- CrawlPackage.dfsFromExposedModules "." solution
                  return ([], summary)

            _ -> CrawlPackage.dfsFromFiles "." solution filePaths

      let summary = CrawlProject.canonicalizePackageSummary Nothing packageSummary

      return
          ( map (\n -> ModuleID n Nothing) moduleNames
          , List.foldl1 CrawlProject.union (summary : summaries)
          )


getSolution :: (MonadIO m, MonadError String m) => m Solution.Solution
getSolution =
  do  exists <- liftIO (doesFileExist Path.solvedDependencies)
      if exists
          then Solution.read Path.solvedDependencies
          else attemptToGenerate
  where
    attemptToGenerate =
        do  exists <- liftIO (doesFileExist Path.description)
            case exists of
              True ->
                  Initialize.solution

              False ->
                  Initialize.descriptionAndSolution




