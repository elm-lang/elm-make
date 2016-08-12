{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified BuildManager as BM
import qualified Flags
import qualified Pipeline.Compile as Compile
import qualified Pipeline.Crawl as Crawl
import qualified Pipeline.Plan as Plan
import qualified Pipeline.Generate as Generate
import TheMasterPlan (ProjectGraph(..), ProjectData(..))


main :: IO ()
main =
  do  setLocaleEncoding utf8
      setNumCapabilities =<< getNumProcessors

      result <- BM.run make
      case result of
        Right (_, _timeline) ->
          do  -- putStrLn (BM.timelineToString _timeline)
              return ()

        Left err ->
          do  BM.printError err
              exitFailure


make :: BM.Task ()
make =
  do  config <- Flags.toConfig

      (Crawl.ProjectInfo thisPackage exposedModules moduleForGeneration projectSummary) <-
          BM.phase "Crawl Project" (Crawl.crawl config)

      let dependencies =
            Map.map projectDependencies (projectData projectSummary)

      let modulesToDocument =
            maybe Set.empty (const exposedModules) (BM._docs config)

      buildSummary <-
          BM.phase "Plan Build" (Plan.planBuild config modulesToDocument projectSummary)

      (interfaces, docs) <-
        BM.phase "Compile" $ liftIO $
          Compile.build
            config
            thisPackage
            exposedModules
            moduleForGeneration
            dependencies
            buildSummary

      BM.phase "Generate Docs" $
        maybe (return ()) (Generate.docs docs) (BM._docs config)

      BM.phase "Generate Code" $
        Generate.generate
          config
          interfaces
          dependencies
          (projectNatives projectSummary)
          moduleForGeneration
