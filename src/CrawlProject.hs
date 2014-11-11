{-# LANGUAGE FlexibleContexts #-}
module CrawlProject where

import qualified Data.Map as Map

import qualified Elm.Compiler.Module as Module
import TheMasterPlan
    ( ModuleID(ModuleID), PackageID, Location(Location)
    , PackageSummary(..), PackageData(..)
    , ProjectSummary(..), ProjectData(..)
    )


canonicalizePackageSummary
    :: PackageID
    -> PackageSummary
    -> ProjectSummary Location
canonicalizePackageSummary package (PackageSummary pkgData natives foreignDependencies) =
    ProjectSummary
    { projectData = 
        Map.map
            (canonicalizePackageData package foreignDependencies)
            (canonicalizeKeys pkgData)
    , projectNatives =
        Map.map (\path -> Location path package) (canonicalizeKeys natives)
    }
  where
    canonicalizeKeys =
        Map.mapKeysMonotonic (\name -> ModuleID name package)


canonicalizePackageData
    :: PackageID
    -> Map.Map Module.Name PackageID
    -> PackageData
    -> ProjectData Location
canonicalizePackageData package foreignDependencies (PackageData filePath deps) =
    ProjectData {
        projectLocation = Location filePath package,
        projectDependencies = map canonicalizeModule deps
    }
  where
    canonicalizeModule :: Module.Name -> ModuleID
    canonicalizeModule moduleName =
        case Map.lookup moduleName foreignDependencies of
          Nothing -> ModuleID moduleName package
          Just foreignPackage ->
              ModuleID moduleName foreignPackage


union :: ProjectSummary a -> ProjectSummary a -> ProjectSummary a
union (ProjectSummary d natives) (ProjectSummary d' natives') =
    ProjectSummary (Map.union d d') (Map.union natives natives')