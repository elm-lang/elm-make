{-# LANGUAGE FlexibleContexts #-}
module Crawl.Packages where

import Control.Monad.Error (MonadError, MonadIO)
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Crawl.Utils as Crawl
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V


type Locations =
    Map.Map Module.Name [N.Name]


locations :: (MonadIO m, MonadError String m) => [(N.Name, V.Version)] -> m Locations
locations packages =
    do  rawLocations <- mapM locate packages
        return (Map.unionsWith (++) rawLocations)


locate :: (MonadIO m, MonadError String m) => (N.Name, V.Version) -> m Locations
locate (pkgName, version) =
    Crawl.within (rootOf pkgName version) $ do
        description <- Desc.read Path.description
        let exposed = Desc.exposed description
        return (foldr insert Map.empty exposed)
  where
    insert moduleName dict =
        Map.insert moduleName [pkgName] dict


rootOf :: N.Name -> V.Version -> FilePath
rootOf name version =
    Path.packagesDirectory </> N.toFilePath name </> V.toString version
