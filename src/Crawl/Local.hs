{-# LANGUAGE FlexibleContexts #-}
module Crawl.Local where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, liftIO)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Crawl.Validate as Validate
import qualified Elm.Compiler.Module as Module


type Locations =
    Map.Map Module.Name [FilePath]


locations :: (MonadIO m) => [FilePath] -> [Module.Name] -> m Locations
locations sourceDirs moduleNames =
  do  pairs <- mapM locate moduleNames
      return (Map.fromList pairs)
  where
    locate name =
        do  paths <- findAllIn sourceDirs name
            return (name, paths)


findAllIn :: (MonadIO m) => [FilePath] -> Module.Name -> m [FilePath]
findAllIn sourceDirs moduleName =
    do  maybeLocations <-
            forM sourceDirs $ \dir -> do
                exists <- liftIO $ doesFileExist (dir </> filePath)
                return (if exists then Just (dir </> filePath) else Nothing)
        return (Maybe.catMaybes maybeLocations)
  where
    filePath =
        Module.nameToPath moduleName <.> "elm"


findIn :: (MonadIO m, MonadError String m) => [FilePath] -> Module.Name -> m FilePath
findIn sourceDirs moduleName =
    do  allDirs <- findAllIn sourceDirs moduleName
        Validate.list id moduleName allDirs
