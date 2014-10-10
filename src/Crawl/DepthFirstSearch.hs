{-# LANGUAGE FlexibleContexts #-}
module Crawl.DepthFirstSearch where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Crawl.Error as Error
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg


-- STATE and ENVIRONMENT

data Env = Env
    { sourceDirs :: [FilePath]
    , exposedModules :: Map.Map Module.Name [Pkg.Name]
    }

data State = State
    { localModules :: Map.Map Module.Name FilePath
    , dependencies :: Map.Map Module.Name [Module.Name]
    , usedPackages :: Set.Set Pkg.Name
    }

initialState :: State
initialState =
    State Map.empty Map.empty Set.empty


-- CRAWLERS

crawlDependencies
    :: (MonadIO m, MonadError String m)
    => [Module.Name] -> Env -> State -> m State

crawlDependencies [] _env state =
    return state

crawlDependencies (name:unvisited) env state =
  do  filePaths <- find (sourceDirs env) name
      case (filePaths, Map.lookup name (exposedModules env)) of
        ([filePath], Nothing) ->
            crawlFile filePath (Just name) unvisited env $ state {
                localModules = Map.insert name filePath (localModules state)
            }

        ([], Just [pkg]) ->
            crawlDependencies unvisited env $ state {
                usedPackages = Set.insert pkg (usedPackages state)
            }

        ([], Nothing) ->
            throwError (Error.notFound name)

        (_, maybePkgs) ->
            throwError (Error.tooMany name (paths ++ pkgs))
          where
            paths = map ("directory " ++) filePaths
            pkgs = map ("package " ++) (Maybe.maybe [] (map Pkg.toString) maybePkgs)


crawlFile
    :: (MonadIO m, MonadError String m)
    => FilePath -> Maybe Module.Name -> [Module.Name] -> Env -> State -> m State

crawlFile path maybeName unvisited env state =
  do  source <- liftIO (readFile path)
      (name, deps) <- Compiler.parseDependencies source

      checkName path name maybeName

      let newUnvisited =
              filter (flip Map.notMember (dependencies state)) deps

      crawlDependencies (unvisited ++ newUnvisited) env $ state {
          dependencies = Map.insert name deps (dependencies state)
      }


-- FIND LOCAL FILE PATH

find :: (MonadIO m) => [FilePath] -> Module.Name -> m [FilePath]
find sourceDirs moduleName =
    do  maybeLocations <-
            forM sourceDirs $ \dir -> do
                exists <- liftIO $ doesFileExist (dir </> filePath)
                return (if exists then Just (dir </> filePath) else Nothing)
        return (Maybe.catMaybes maybeLocations)
  where
    filePath =
        Module.nameToPath moduleName <.> "elm"


-- CHECK MODULE NAME MATCHES FILE NAME

checkName
    :: (MonadError String m)
    => FilePath -> Module.Name -> Maybe Module.Name -> m ()
checkName path nameFromSource maybeName =
    case maybeName of
      Nothing -> return ()
      Just nameFromPath
        | nameFromSource == nameFromPath -> return ()
        | otherwise ->
            throwError (Error.nameMismatch path nameFromPath nameFromSource)
