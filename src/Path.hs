module Path (toInterface, toObjectFile, toPackageCacheFile, toSource) where

import qualified Data.List as List
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package as Pkg
import qualified TheMasterPlan as TMP


toInterface :: FilePath -> TMP.CanonicalModule -> FilePath
toInterface root (TMP.CanonicalModule package (Module.Name names)) =
    root </> inPackage package (List.intercalate "-" names <.> "elmi")


toObjectFile :: FilePath -> TMP.CanonicalModule -> FilePath
toObjectFile root (TMP.CanonicalModule package (Module.Name names)) =
    root </> inPackage package (List.intercalate "-" names <.> "elmo")


toPackageCacheFile :: FilePath -> TMP.Package -> FilePath
toPackageCacheFile root pkg =
    root </> inPackage pkg "graph.dat"


toSource :: TMP.Location -> FilePath
toSource (TMP.Location relativePath _package) =
    relativePath


inPackage :: TMP.Package -> FilePath -> FilePath
inPackage (name, version) relativePath =
    Pkg.toFilePath name </> Pkg.versionToString version </> relativePath
