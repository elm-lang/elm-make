{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, runErrorT, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (dropFileName)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified Build
import qualified CrawlPackage
import qualified CrawlProject
import qualified LoadInterfaces
import qualified Options
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Solver as Solver
import qualified Path as BuildPath
import TheMasterPlan (Location, ProjectSummary(..), ProjectData(..))


main :: IO ()
main =
  do  option <- Options.parse
      case option of
        Options.BuildPackage -> return ()
        Options.BuildFile path -> return ()

      result <- runErrorT (runReaderT run "cache")
      case result of
        Right () -> return ()
        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure


run :: (MonadIO m, MonadError String m, MonadReader FilePath m) => m ()
run =
  do  numProcessors <- liftIO getNumProcessors
      liftIO (setNumCapabilities numProcessors)

      projectSummary <- crawl
      let dependencies = Map.map projectDependencies (projectData projectSummary)
      buildSummary <- LoadInterfaces.prepForBuild projectSummary

      cachePath <- ask
      liftIO (Build.build numProcessors cachePath dependencies buildSummary)


crawl :: (MonadIO m, MonadError String m) => m (ProjectSummary Location)
crawl =
  do  solution <- getSolution

      summaries <-
          forM (Map.toList solution) $ \(name,version) -> do
              packageSummary <- CrawlPackage.crawl (BuildPath.fromPackage name version) solution Nothing
              return (CrawlProject.canonicalizePackageSummary (Just (name,version)) packageSummary)

      summary <-
          do  packageSummary <- CrawlPackage.crawl "." solution Nothing
              return (CrawlProject.canonicalizePackageSummary Nothing packageSummary)

      return (List.foldl1 CrawlProject.union (summary : summaries))


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
              False ->
                return Map.empty

              True ->
                do  description <- Desc.read Path.description
                    solution <- Solver.solve (Desc.dependencies description)
                    liftIO $ do
                        createDirectoryIfMissing True (dropFileName Path.solvedDependencies)
                        Solution.write Path.solvedDependencies solution
                    return solution






