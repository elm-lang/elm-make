{-# LANGUAGE FlexibleContexts #-}
module Path where

import qualified Data.List as List
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package.Name as Pkg
import Elm.Package.Version as V
import qualified TheMasterPlan as TMP


toInterface :: FilePath -> TMP.CanonicalModule -> FilePath
toInterface root (TMP.CanonicalModule package (Module.Name names)) =
    root </> inPackage package (List.intercalate "-" names <.> "elmi")


toObjectFile :: FilePath -> TMP.CanonicalModule -> FilePath
toObjectFile root (TMP.CanonicalModule package (Module.Name names)) =
    root </> inPackage package (List.intercalate "-" names <.> "elmo")


toSource :: TMP.Location -> FilePath
toSource (TMP.Location relativePath _package) =
    relativePath


inPackage :: TMP.Package -> FilePath -> FilePath
inPackage (name, version) relativePath =
    fromPackage name version </> relativePath


fromPackage :: Pkg.Name -> V.Version -> FilePath
fromPackage name version =
    Pkg.toFilePath name </> V.toString version