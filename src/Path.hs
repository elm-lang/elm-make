{-# LANGUAGE FlexibleContexts #-}
module Path where

import Control.Monad.RWS (MonadReader, ask)
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package.Name as Pkg
import TheMasterPlan (ModuleID(ModuleID), Location(Location))


fromModuleID :: (MonadReader FilePath m) => ModuleID -> m FilePath
fromModuleID (ModuleID pkgName moduleName) =
  do  root <- ask
      return (root </> pkgPath </> modulePath <.> "elmi")
  where
    pkgPath = Pkg.toFilePath pkgName
    modulePath = Module.nameToPath moduleName


fromLocation :: Location -> FilePath
fromLocation (Location package relativePath) =
    Pkg.toFilePath package </> error "version" </> relativePath