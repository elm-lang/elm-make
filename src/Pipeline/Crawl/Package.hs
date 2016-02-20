{-# OPTIONS_GHC -Wall #-}
module Pipeline.Crawl.Package where

import Control.Arrow (second)
import Control.Monad.Except (liftIO, throwError, withExceptT)
import qualified Data.Map as Map
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), (<.>))

import qualified BuildManager as BM
import qualified Utils.File as File
import TheMasterPlan ( PackageGraph(..), PackageData(..) )



-- STATE and ENVIRONMENT


data Env = Env
    { _sourceDirs :: [FilePath]
    , _availableForeignModules :: Map.Map Module.Raw [(Pkg.Name, Pkg.Version)]
    }


initEnv :: FilePath -> Desc.Description -> Solution.Solution -> BM.Task Env
initEnv root desc solution =
  do  availableForeignModules <- readAvailableForeignModules desc solution
      let sourceDirs = map (root </>) (Desc.sourceDirs desc)
      return (Env sourceDirs availableForeignModules)



-- GENERIC CRAWLER


dfsFromFiles
    :: FilePath
    -> Solution.Solution
    -> Desc.Description
    -> [FilePath]
    -> BM.Task ([Module.Raw], PackageGraph)

dfsFromFiles root solution desc filePaths =
  do  env <- initEnv root desc solution

      let pkgName = Desc.name desc
      info <- mapM (readPackageData pkgName Nothing) filePaths
      let names = map fst info
      let unvisited = concatMap (snd . snd) info
      let pkgData = Map.fromList (map (second fst) info)
      let initialGraph = PackageGraph pkgData Map.empty Map.empty

      summary <-
          dfs (Desc.natives desc) pkgName unvisited env initialGraph

      return (names, summary)


dfsFromExposedModules
    :: FilePath
    -> Solution.Solution
    -> Desc.Description
    -> BM.Task PackageGraph
dfsFromExposedModules root solution desc =
  do  env <- initEnv root desc solution
      let unvisited = map (Unvisited Nothing) (Desc.exposed desc)
      let summary = PackageGraph Map.empty Map.empty Map.empty
      dfs (Desc.natives desc) (Desc.name desc) unvisited env summary




-- DEPTH FIRST SEARCH


data Unvisited =
  Unvisited
    { _parent :: Maybe Module.Raw
    , _name :: Module.Raw
    }


dfs :: Bool -> Pkg.Name -> [Unvisited] -> Env -> PackageGraph -> BM.Task PackageGraph
dfs allowNatives pkgName unvisited env summary =
  case unvisited of
    [] ->
      return summary

    next@(Unvisited _ name) : rest ->
      if Map.member name (packageData summary) then
        dfs allowNatives pkgName rest env summary

      else
        dfsHelp allowNatives pkgName next rest env summary


dfsHelp :: Bool -> Pkg.Name -> Unvisited -> [Unvisited] -> Env -> PackageGraph -> BM.Task PackageGraph
dfsHelp allowNatives pkgName (Unvisited maybeParent name) unvisited env summary =
  do  -- find all paths that match the unvisited module name
      filePaths <-
        find allowNatives name (_sourceDirs env)

      -- see if we found a unique path for the name
      case (filePaths, Map.lookup name (_availableForeignModules env)) of
        ([Elm filePath], Nothing) ->
            do  (statedName, (pkgData, newUnvisited)) <-
                    readPackageData pkgName (Just name) filePath

                dfs allowNatives pkgName (newUnvisited ++ unvisited) env $ summary {
                    packageData = Map.insert statedName pkgData (packageData summary)
                }

        ([JS filePath], Nothing) ->
            dfs allowNatives pkgName unvisited env $ summary {
                packageNatives = Map.insert name filePath (packageNatives summary)
            }

        ([], Just [pkg]) ->
            dfs allowNatives pkgName unvisited env $ summary {
                packageForeignDependencies =
                    Map.insert name pkg (packageForeignDependencies summary)
            }

        ([], Nothing) ->
            throwError (BM.ModuleNotFound name maybeParent)

        (_, maybePkgs) ->
            throwError $
              BM.ModuleDuplicates
                name
                maybeParent
                (map toFilePath filePaths)
                (maybe [] (map fst) maybePkgs)



-- FIND LOCAL FILE PATH


data CodePath = Elm FilePath | JS FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm file -> file
    JS file -> file


find :: Bool -> Module.Raw -> [FilePath] -> BM.Task [CodePath]
find allowNatives moduleName sourceDirs =
    findHelp allowNatives [] moduleName sourceDirs


findHelp :: Bool -> [CodePath] -> Module.Raw -> [FilePath] -> BM.Task [CodePath]
findHelp _allowNatives locations _moduleName [] =
  return locations

findHelp allowNatives locations moduleName (dir:srcDirs) =
  do  locations' <- addElmPath locations
      updatedLocations <-
          if allowNatives then addJsPath locations' else return locations'
      findHelp allowNatives updatedLocations moduleName srcDirs
  where
    consIf bool x xs =
        if bool then x:xs else xs

    addElmPath locs =
      do  let elmPath = dir </> Module.nameToPath moduleName <.> "elm"
          elmExists <- liftIO (doesFileExist elmPath)
          return (consIf elmExists (Elm elmPath) locs)

    addJsPath locs =
      do  let jsPath = dir </> Module.nameToPath moduleName <.> "js"
          jsExists <-
              case moduleName of
                "Native" : _ ->
                  liftIO (doesFileExist jsPath)

                _ ->
                  return False

          return (consIf jsExists (JS jsPath) locs)



-- READ and VALIDATE PACKAGE DATA for an ELM file


readPackageData
    :: Pkg.Name
    -> Maybe Module.Raw
    -> FilePath
    -> BM.Task (Module.Raw, (PackageData, [Unvisited]))
readPackageData pkgName maybeName filePath =
  do  sourceCode <- liftIO (File.readStringUtf8 filePath)

      (name, rawDeps) <-
          case Compiler.parseDependencies sourceCode of
            Right result ->
                return result
            Left msgs ->
                throwError (BM.CompilerErrors filePath sourceCode msgs)

      checkName filePath name maybeName

      let deps =
            if pkgName == Pkg.coreName
              then rawDeps
              else Module.defaultImports ++ rawDeps

      return
        ( name
        , ( PackageData filePath deps
          , map (Unvisited (Just name)) deps
          )
        )


checkName :: FilePath -> Module.Raw -> Maybe Module.Raw -> BM.Task ()
checkName path nameFromSource maybeName =
  case maybeName of
    Just nameFromPath | nameFromSource /= nameFromPath ->
      throwError (BM.ModuleName path nameFromPath nameFromSource)

    _ ->
      return ()



-- FOREIGN MODULES -- which ones are available, who exposes them?


readAvailableForeignModules
    :: Desc.Description
    -> Solution.Solution
    -> BM.Task (Map.Map Module.Raw [(Pkg.Name, Pkg.Version)])
readAvailableForeignModules desc solution =
  do  visiblePackages <- allVisible desc solution
      rawLocations <- mapM exposedModules visiblePackages
      return (Map.unionsWith (++) rawLocations)


allVisible
    :: Desc.Description
    -> Solution.Solution
    -> BM.Task [(Pkg.Name, Pkg.Version)]
allVisible desc solution =
    mapM getVersion visible
  where
    visible =
        map fst (Desc.dependencies desc)

    getVersion :: Pkg.Name -> BM.Task (Pkg.Name, Pkg.Version)
    getVersion name =
        case Map.lookup name solution of
          Just version ->
              return (name, version)

          Nothing ->
              throwError (BM.MissingPackage name)


exposedModules
    :: (Pkg.Name, Pkg.Version)
    -> BM.Task (Map.Map Module.Raw [(Pkg.Name, Pkg.Version)])
exposedModules packageID@(pkgName, version) =
    within (Path.package pkgName version) $ do
        description <- withExceptT BM.PackageProblem (Desc.read Path.description)
        let exposed = Desc.exposed description
        return (foldr insert Map.empty exposed)
  where
    insert moduleName dict =
        Map.insert moduleName [packageID] dict


within :: FilePath -> BM.Task a -> BM.Task a
within directory command =
    do  root <- liftIO getCurrentDirectory
        liftIO (setCurrentDirectory directory)
        result <- command
        liftIO (setCurrentDirectory root)
        return result

