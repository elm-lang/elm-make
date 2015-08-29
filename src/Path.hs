module Path (toInterface, toObjectFile, toPackageCacheFile, toSource) where

import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package as Pkg
import qualified TheMasterPlan as TMP


toInterface :: FilePath -> TMP.CanonicalModule -> FilePath
toInterface root (TMP.CanonicalModule package name) =
    root </> inPackage package (Module.hyphenate name <.> "elmi")


toObjectFile :: FilePath -> TMP.CanonicalModule -> FilePath
toObjectFile root (TMP.CanonicalModule package name) =
    root </> inPackage package (Module.hyphenate name <.> "elmo")


toPackageCacheFile :: FilePath -> TMP.Package -> FilePath
toPackageCacheFile root pkg =
    root </> inPackage pkg "graph.dat"


toSource :: TMP.Location -> FilePath
toSource (TMP.Location relativePath _package) =
    relativePath


inPackage :: TMP.Package -> FilePath -> FilePath
inPackage (name, version) relativePath =
    Pkg.toFilePath name </> Pkg.versionToString version </> relativePath
