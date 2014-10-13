{-# LANGUAGE FlexibleContexts #-}
module CrawlProject where

import qualified Data.Map as Map

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import TheMasterPlan
    ( ModuleID(ModuleID), Location(Location)
    , PackageSummary(..), PackageData(..)
    , ProjectSummary, ProjectData(..)
    )


canonicalizePackageSummary
    :: Pkg.Name
    -> Map.Map Module.Name Pkg.Name
    -> PackageSummary
    -> ProjectSummary Location
canonicalizePackageSummary pkgName foreignDependencies pkgSummary =
    Map.map
        (canonicalizePackageData pkgName foreignDependencies)
        (Map.mapKeysMonotonic (ModuleID pkgName) (packageData pkgSummary))


canonicalizePackageData
    :: Pkg.Name
    -> Map.Map Module.Name Pkg.Name
    -> PackageData
    -> ProjectData Location
canonicalizePackageData pkgName foreignDependencies (PackageData filePath deps) =
    ProjectData {
        projectLocation = Location pkgName filePath,
        projectDependencies = map canonicalizeModule deps
    }
  where
    canonicalizeModule :: Module.Name -> ModuleID
    canonicalizeModule moduleName =
        case Map.lookup moduleName foreignDependencies of
          Nothing -> ModuleID pkgName moduleName
          Just foreignPackage ->
              ModuleID foreignPackage moduleName


