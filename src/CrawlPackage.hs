{-# LANGUAGE FlexibleContexts #-}
module CrawlPackage where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V
import TheMasterPlan (PackageSummary(..))


-- STATE and ENVIRONMENT

data Env = Env
    { sourceDirs :: [FilePath]
    , availableForeignModules :: Map.Map Module.Name [Pkg.Name]
    }

emptyPackageSummary :: PackageSummary
emptyPackageSummary =
    PackageSummary Map.empty Map.empty Map.empty


-- GENERIC CRAWLER

crawl :: (MonadIO m, MonadError String m) => FilePath -> Solution.Solution -> Maybe FilePath -> m PackageSummary
crawl root solution maybeFilePath =
  do  desc <- Desc.read (root </> Path.description)

      availableForeignModules <- readAvailableForeignModules desc solution
      let sourceDirs = map (root </>) (Desc.sourceDirs desc)
      let env = Env sourceDirs availableForeignModules

      case maybeFilePath of
        Just path ->
            dfsFile path Nothing [] env emptyPackageSummary
        Nothing ->
            dfsDependencies (Desc.exposed desc) env emptyPackageSummary


-- DEPTH FIRST SEARCH

dfsDependencies
    :: (MonadIO m, MonadError String m)
    => [Module.Name] -> Env -> PackageSummary -> m PackageSummary

dfsDependencies [] _env summary =
    return summary

dfsDependencies (name:unvisited) env summary =
  do  filePaths <- find (sourceDirs env) name
      case (filePaths, Map.lookup name (availableForeignModules env)) of
        ([filePath], Nothing) ->
            dfsFile filePath (Just name) unvisited env $ summary {
                packageLocations = Map.insert name filePath (packageLocations summary)
            }

        ([], Just [pkg]) ->
            dfsDependencies unvisited env $ summary {
                foreignDependencies = Map.insert name pkg (foreignDependencies summary)
            }

        ([], Nothing) ->
            throwError (errorNotFound name)

        (_, maybePkgs) ->
            throwError (errorTooMany name (paths ++ pkgs))
          where
            paths = map ("directory " ++) filePaths
            pkgs = map ("package " ++) (Maybe.maybe [] (map Pkg.toString) maybePkgs)


dfsFile
    :: (MonadIO m, MonadError String m)
    => FilePath -> Maybe Module.Name -> [Module.Name] -> Env -> PackageSummary -> m PackageSummary

dfsFile path maybeName unvisited env summary =
  do  source <- liftIO (readFile path)
      (name, deps) <- Compiler.parseDependencies source

      checkName path name maybeName

      let newUnvisited =
              filter (flip Map.notMember (packageDependencies summary)) deps

      dfsDependencies (unvisited ++ newUnvisited) env $ summary {
          packageDependencies = Map.insert name deps (packageDependencies summary)
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
            throwError (errorNameMismatch path nameFromPath nameFromSource)


-- FOREIGN MODULES -- which ones are available, who exposes them?

readAvailableForeignModules
    :: (MonadIO m, MonadError String m)
    => Desc.Description
    -> Solution.Solution
    -> m (Map.Map Module.Name [Pkg.Name])
readAvailableForeignModules desc solution =
  do  rawLocations <-
          mapM exposedModules (allVisible desc solution)
      return (Map.unionsWith (++) rawLocations)


allVisible :: Desc.Description -> Solution.Solution -> [(Pkg.Name, V.Version)]
allVisible desc solution =
    map (\name -> (name, solution ! name)) visible
  where
    visible = map fst (Desc.dependencies desc)


exposedModules
    :: (MonadIO m, MonadError String m)
    => (Pkg.Name, V.Version)
    -> m (Map.Map Module.Name [Pkg.Name])
exposedModules (pkgName, version) =
    within (Path.package pkgName version) $ do
        description <- Desc.read Path.description
        let exposed = Desc.exposed description
        return (foldr insert Map.empty exposed)
  where
    insert moduleName dict =
        Map.insert moduleName [pkgName] dict


within :: (MonadIO m) => FilePath -> m a -> m a
within directory command =
    do  root <- liftIO getCurrentDirectory
        liftIO (setCurrentDirectory directory)
        result <- command
        liftIO (setCurrentDirectory root)
        return result


-- ERROR MESSAGES

errorNotFound :: Module.Name -> String
errorNotFound name =
    unlines
    [ "could not find module '" ++ Module.nameToString name ++ "'"
    , ""
    , "Potential problems could be:"
    , "  * Misspelled the module name"
    , "  * Need to add a source directory or new dependency to " ++ Path.description
    ]


errorTooMany :: Module.Name -> [String] -> String
errorTooMany name searchedList =
    "found multiple modules named '" ++ Module.nameToString name ++ "'\n"
    ++ "Modules with that name were found in the following locations:\n\n"
    ++ concatMap (\str -> "    " ++ str ++ "\n") searchedList


errorNameMismatch :: FilePath -> Module.Name -> Module.Name -> String
errorNameMismatch path nameFromPath nameFromSource =
    unlines
    [ "The module name is messed up for " ++ path
    , "    According to the file's name it should be " ++ Module.nameToString nameFromPath
    , "    According to the source code it should be " ++ Module.nameToString nameFromSource
    , "Which is it?"
    ]
