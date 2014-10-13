{-# LANGUAGE FlexibleContexts #-}
module CrawlProject where

import qualified Data.Map as Map

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import TheMasterPlan
    ( ModuleID(ModuleID), PackageID, Location(Location)
    , PackageSummary(..), PackageData(..)
    , ProjectSummary, ProjectData(..)
    )


canonicalizePackageSummary
    :: Maybe PackageID
    -> PackageSummary
    -> ProjectSummary Location
canonicalizePackageSummary package (PackageSummary pkgData foreignDependencies) =
    Map.map
        (canonicalizePackageData package foreignDependencies)
        (Map.mapKeysMonotonic (\name -> ModuleID name package) pkgData)


canonicalizePackageData
    :: Maybe PackageID
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
              ModuleID moduleName (Just foreignPackage)


