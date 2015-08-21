{-# OPTIONS_GHC -Wall #-}
module Pipeline.Crawl where

import Control.Monad (forM)
import Control.Monad.Except (liftIO, withExceptT)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Initialize as Initialize
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import qualified BuildManager as BM
import qualified Pipeline.Crawl.Package as CrawlPackage
import TheMasterPlan
    ( CanonicalModule(CanonicalModule), Package, Location(Location)
    , PackageGraph(..), PackageData(..)
    , ProjectGraph(..), ProjectData(..)
    )


data ProjectInfo = ProjectInfo
    { _package :: Package
    , _exposedModules :: Set.Set CanonicalModule
    , _allModules :: [CanonicalModule]
    , _summary :: ProjectGraph Location
    }


crawl :: BM.Config -> BM.Task ProjectInfo
crawl config =
  do  solution <- getSolution (BM._autoYes config)

      summaries <-
          forM (Map.toList solution) $ \(name,version) ->
            BM.phase (Pkg.toString name) $ do
              let root = Path.package name version
              desc <- withExceptT BM.PackageProblem (Desc.read (root </> Path.description))
              packageGraph <- CrawlPackage.dfsFromExposedModules root solution desc
              return (canonicalizePackageGraph (name,version) packageGraph)


      desc <- withExceptT BM.PackageProblem (Desc.read Path.description)

      (moduleForGeneration, packageGraph) <-
          CrawlPackage.dfsFromFiles "." solution desc (BM._files config)

      let thisPackage =
            (Desc.name desc, Desc.version desc)

      let summary =
            canonicalizePackageGraph thisPackage packageGraph

      let localize moduleName =
            CanonicalModule thisPackage moduleName

      return $ ProjectInfo
          thisPackage
          (Set.fromList (map localize (Desc.exposed desc)))
          (map localize moduleForGeneration)
          (List.foldl1 union (summary : summaries))


getSolution :: Bool -> BM.Task Solution.Solution
getSolution autoYes =
  do  exists <- liftIO (doesFileExist Path.solvedDependencies)
      withExceptT BM.PackageProblem $
        if exists then
          Solution.read Path.solvedDependencies
        else
          Initialize.solution autoYes


canonicalizePackageGraph
    :: Package
    -> PackageGraph
    -> ProjectGraph Location
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
    -> Map.Map Module.Name Package
    -> PackageData
    -> ProjectData Location
canonicalizePackageData package foreignDependencies (PackageData filePath deps) =
    ProjectData {
        projectLocation = Location filePath package,
        projectDependencies = map canonicalizeModule deps
    }
  where
    canonicalizeModule :: Module.Name -> CanonicalModule
    canonicalizeModule moduleName =
        case Map.lookup moduleName foreignDependencies of
          Nothing ->
              CanonicalModule package moduleName
          Just foreignPackage ->
              CanonicalModule foreignPackage moduleName


union :: ProjectGraph a -> ProjectGraph a -> ProjectGraph a
union (ProjectGraph d natives) (ProjectGraph d' natives') =
    ProjectGraph (Map.union d d') (Map.union natives natives')