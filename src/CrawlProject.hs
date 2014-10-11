{-# LANGUAGE FlexibleContexts #-}
module CrawlProject where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V
import TheMasterPlan
    ( ModuleID(ModuleID), Location(Location)
    , PackageSummary(..), PackageData(..)
    , ProjectSummary, ProjectData(..)
    )


canonicalizePackageSummary
    :: Pkg.Name -> Map.Map Module.Name Pkg.Name -> PackageSummary -> ProjectSummary
canonicalizePackageSummary pkgName foreignDependencies pkgSummary =
    Map.map
        (canonicalizePackageData pkgName foreignDependencies)
        (Map.mapKeysMonotonic (ModuleID pkgName) (packageData pkgSummary))


canonicalizePackageData
    :: Pkg.Name -> Map.Map Module.Name Pkg.Name -> PackageData -> ProjectData
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


