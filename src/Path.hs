module Path where

import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module
import Elm.Package.Name as Pkg


toInterface :: Maybe Pkg.Name -> Module.Name -> FilePath
toInterface maybePackage moduleName =
    root </> interfacePath
  where
    interfacePath =
        Module.nameToPath moduleName <.> "elmi"

    root =
        case maybePackage of
          Nothing -> 
              artifactsDirectory

          Just pkgName ->
              artifactsDirectory </> Pkg.toFilePath pkgName


artifactsDirectory :: FilePath
artifactsDirectory =
    "elm_artifacts"