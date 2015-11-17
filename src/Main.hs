{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified BuildManager as BM
import qualified Flags
import qualified Pipeline.Compile as Compile
import qualified Pipeline.Crawl as Crawl
import qualified Pipeline.Plan as Plan
import qualified Pipeline.Generate as Generate
import TheMasterPlan (ProjectGraph(..), ProjectData(..))


main :: IO ()
main =
  do  numProcessors <- getNumProcessors
      setNumCapabilities numProcessors

      result <- BM.run (make numProcessors)
      case result of
        Right (_, _timeline) ->
          -- putStrLn (BM.timelineToString timeline)
          return ()

        Left err ->
          do  BM.printError err
              exitFailure


make :: Int -> BM.Task ()
make numProcessors =
  do  config <- Flags.toConfig

      (Crawl.ProjectInfo thisPackage exposedModules moduleForGeneration projectSummary) <-
          BM.phase "Crawl Project" (Crawl.crawl config)

      let dependencies =
            Map.map projectDependencies (projectData projectSummary)

      let modulesToDocument =
            maybe Set.empty (const exposedModules) (BM._docs config)

      buildSummary <-
          BM.phase "Plan Build" (Plan.planBuild config modulesToDocument projectSummary)

      docs <-
        BM.phase "Compile" $ liftIO $
          Compile.build
            config
            numProcessors
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
          dependencies
          (projectNatives projectSummary)
          moduleForGeneration

