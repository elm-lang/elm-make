{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, runErrorT, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified Build
import qualified CrawlPackage
import qualified CrawlProject
import qualified LoadInterfaces
import qualified Options
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
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
      let dependencies = Map.map projectDependencies projectSummary
      buildSummary <- LoadInterfaces.prepForBuild projectSummary

      liftIO (Build.build numProcessors dependencies buildSummary)


crawl :: (MonadIO m, MonadError String m) => m (ProjectSummary Location)
crawl =
  do  solution <- Solution.read Path.solvedDependencies

      summaries <-
          forM (Map.toList solution) $ \(name,version) -> do
              packageSummary <- CrawlPackage.crawl (BuildPath.fromPackage name version) solution Nothing
              return (CrawlProject.canonicalizePackageSummary name packageSummary)

      summary <-
          do  packageSummary <- CrawlPackage.crawl "." solution Nothing
              return (CrawlProject.canonicalizePackageSummary Pkg.dummyName packageSummary)

      return (Map.unions (summary : summaries))