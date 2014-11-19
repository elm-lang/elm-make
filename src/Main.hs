{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, runErrorT, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified Build
import qualified CrawlPackage
import qualified CrawlProject
import qualified LoadInterfaces
import qualified Arguments
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Initialize as Initialize
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Generate
import TheMasterPlan
    ( ModuleID(ModuleID), Location, PackageID
    , ProjectSummary(..), ProjectData(..)
    )


main :: IO ()
main =
  do  args <- Arguments.parse

      result <- runErrorT (runReaderT (run args) artifactDirectory)
      case result of
        Right () ->
          return ()

        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure


artifactDirectory :: FilePath
artifactDirectory =
    Path.stuffDirectory </> "build-artifacts"


run :: (MonadIO m, MonadError String m, MonadReader FilePath m)
    => Arguments.Arguments
    -> m ()
run args =
  do  numProcessors <- liftIO getNumProcessors
      liftIO (setNumCapabilities numProcessors)

      (thisPackage, publicModules, projectSummary) <-
          crawl (Arguments.autoYes args) (Arguments.files args)

      let dependencies = Map.map projectDependencies (projectData projectSummary)
      buildSummary <- LoadInterfaces.prepForBuild projectSummary

      cachePath <- ask
      liftIO $
        Build.build
            numProcessors
            thisPackage
            cachePath
            publicModules
            dependencies
            buildSummary

      Generate.generate
          cachePath
          dependencies
          (projectNatives projectSummary)
          publicModules
          (maybe "elm.js" id (Arguments.outputFile args))


crawl
    :: (MonadIO m, MonadError String m)
    => Bool
    -> [FilePath]
    -> m (PackageID, [ModuleID], ProjectSummary Location)
crawl autoYes filePaths =
  do  solution <- getSolution autoYes

      summaries <-
          forM (Map.toList solution) $ \(name,version) -> do
              let root = Path.package name version
              desc <- Desc.read (root </> Path.description)
              packageSummary <- CrawlPackage.dfsFromExposedModules root solution desc
              return (CrawlProject.canonicalizePackageSummary (name,version) packageSummary)


      desc <- Desc.read Path.description
      
      (moduleNames, packageSummary) <-
          case filePaths of
            [] ->
              do  summary <- CrawlPackage.dfsFromExposedModules "." solution desc
                  return ([], summary)

            _ -> CrawlPackage.dfsFromFiles "." solution desc filePaths

      let thisPackage =
            (Desc.name desc, Desc.version desc)

      let summary =
            CrawlProject.canonicalizePackageSummary thisPackage packageSummary

      return
          ( thisPackage
          , map (\n -> ModuleID n thisPackage) moduleNames
          , List.foldl1 CrawlProject.union (summary : summaries)
          )


getSolution :: (MonadIO m, MonadError String m) => Bool -> m Solution.Solution
getSolution autoYes =
  do  exists <- liftIO (doesFileExist Path.solvedDependencies)
      if exists
          then Solution.read Path.solvedDependencies
          else Initialize.solution autoYes

