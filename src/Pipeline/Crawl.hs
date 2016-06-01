{-# OPTIONS_GHC -Wall #-}
module Pipeline.Crawl where

import Control.Monad.Except (catchError, liftIO, withExceptT)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Initialize as Initialize
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import qualified BuildManager as BM
import qualified Path
import qualified Pipeline.Crawl.Package as CrawlPackage
import TheMasterPlan
    ( CanonicalModule(CanonicalModule), Package, Location(Location)
    , PackageGraph(..), PackageData(..)
    , ProjectGraph(..), ProjectData(..)
    )
import qualified Utils.File as File


data ProjectInfo = ProjectInfo
    { _package :: Package
    , _exposedModules :: Set.Set CanonicalModule
    , _allModules :: [CanonicalModule]
    , _graph :: ProjectGraph Location
    }


crawl :: BM.Config -> BM.Task ProjectInfo
crawl config =
  do  solution <- getSolution (BM._autoYes config)

      depGraphs <- mapM (crawlDependency config solution) (Map.toList solution)

      desc <- (Desc.read BM.PackageProblem Path.description)

      let permits = BM._permissions config

      (moduleForGeneration, packageGraph) <-
        case BM._files config of
          [] ->
            (,) [] <$> CrawlPackage.dfsFromExposedModules "." solution desc permits

          filePaths ->
            CrawlPackage.dfsFromFiles "." solution desc permits filePaths

      let thisPackage =
            (Desc.name desc, Desc.version desc)

      let graph =
            canonicalizePackageGraph thisPackage packageGraph

      let localize moduleName =
            CanonicalModule thisPackage moduleName

      return $ ProjectInfo
          thisPackage
          (Set.fromList (map localize (Desc.exposed desc)))
          (map localize moduleForGeneration)
          (List.foldl1 union (graph : depGraphs))


getSolution :: Bool -> BM.Task Solution.Solution
getSolution autoYes =
  do  exists <- liftIO (doesFileExist Path.solvedDependencies)
      if exists
        then Solution.read BM.PackageProblem Path.solvedDependencies
        else withExceptT BM.PackageProblem (Initialize.solution autoYes)


crawlDependency
  :: BM.Config
  -> Solution.Solution
  -> Package
  -> BM.Task (ProjectGraph Location)
crawlDependency config solution pkg@(name,version) =
  let
    root = Path.package name version
    cache = Path.toPackageCacheFile (BM._artifactDirectory config) pkg
  in
    BM.phase (Pkg.toString name) $
      File.readBinary cache `catchError` \_ -> do
          desc <- Desc.read BM.PackageProblem (root </> Path.description)
          packageGraph <- CrawlPackage.dfsFromExposedModules root solution desc BM.Effects
          let projectGraph = canonicalizePackageGraph (name,version) packageGraph
          liftIO (File.writeBinary cache projectGraph)
          return projectGraph


canonicalizePackageGraph :: Package -> PackageGraph -> ProjectGraph Location
canonicalizePackageGraph package (PackageGraph pkgData natives foreignDependencies) =
    ProjectGraph
    { projectData =
        Map.map
            (canonicalizePackageData package foreignDependencies)
            (canonicalizeKeys pkgData)
    , projectNatives =
        Map.map (\path -> Location path package) (canonicalizeKeys natives)
    }
  where
    canonicalizeKeys =
        Map.mapKeys (CanonicalModule package)


canonicalizePackageData
  :: Package
  -> Map.Map Module.Raw Package
  -> PackageData
  -> ProjectData Location
canonicalizePackageData package foreignDependencies (PackageData filePath deps) =
    ProjectData {
        projectLocation = Location filePath package,
        projectDependencies = map canonicalizeModule deps
    }
  where
    canonicalizeModule :: Module.Raw -> CanonicalModule
    canonicalizeModule moduleName =
        case Map.lookup moduleName foreignDependencies of
          Nothing ->
              CanonicalModule package moduleName
          Just foreignPackage ->
              CanonicalModule foreignPackage moduleName


union :: ProjectGraph a -> ProjectGraph a -> ProjectGraph a
union (ProjectGraph d natives) (ProjectGraph d' natives') =
    ProjectGraph (Map.union d d') (Map.union natives natives')
