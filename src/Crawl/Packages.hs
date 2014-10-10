{-# LANGUAGE FlexibleContexts #-}
module Crawl.Packages where

import Control.Monad.Error (MonadError, MonadIO)
import Data.Map ((!))
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Crawl.Utils as Crawl
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Elm.Package.Version as V


allVisible :: Desc.Description -> S.Solution -> [(N.Name, V.Version)]
allVisible desc solution =
    map (\name -> (name, solution ! name)) visible
  where
    visible = map fst (Desc.dependencies desc)


allExposedModules
    :: (MonadIO m, MonadError String m)
    => Desc.Description
    -> S.Solution
    -> m (Map.Map Module.Name [N.Name])
allExposedModules desc solution =
  do  rawLocations <-
          mapM exposedModules (allVisible desc solution)
      return (Map.unionsWith (++) rawLocations)


exposedModules
    :: (MonadIO m, MonadError String m)
    => (N.Name, V.Version)
    -> m (Map.Map Module.Name [N.Name])
exposedModules (pkgName, version) =
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
