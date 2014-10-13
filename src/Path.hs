{-# LANGUAGE FlexibleContexts #-}
module Path where

import Control.Monad.RWS (MonadReader, ask)
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package.Name as Pkg
import Elm.Package.Version as V
import TheMasterPlan (ModuleID(ModuleID), Location(Location))


fromModuleID :: (MonadReader FilePath m) => ModuleID -> m FilePath
fromModuleID (ModuleID moduleName package) =
  do  root <- ask
      return $ root </> inPackage package (modulePath <.> "elmi")
  where
    modulePath = Module.nameToPath moduleName


fromLocation :: Location -> FilePath
fromLocation (Location relativePath package) =
    inPackage package relativePath


inPackage :: Maybe (Pkg.Name, V.Version) -> FilePath -> FilePath
inPackage packageID relativePath =
    case packageID of
      Nothing -> relativePath
      Just (name, version) ->
          fromPackage name version </> relativePath


fromPackage :: Pkg.Name -> V.Version -> FilePath
fromPackage name version =
    Pkg.toFilePath name </> V.toString version