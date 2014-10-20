{-# LANGUAGE FlexibleContexts #-}
module Path where

import qualified Data.List as List
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package.Name as Pkg
import Elm.Package.Version as V
import TheMasterPlan (ModuleID(ModuleID), Location(Location))


toInterface :: FilePath -> ModuleID -> FilePath
toInterface root (ModuleID (Module.Name names) package) =
    root </> inPackage package (List.intercalate "-" names <.> "elmi")


toObjectFile :: FilePath -> ModuleID -> FilePath
toObjectFile root (ModuleID (Module.Name names) package) =
    root </> inPackage package (List.intercalate "-" names <.> "elmo")


toSource :: Location -> FilePath
toSource (Location relativePath package) =
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