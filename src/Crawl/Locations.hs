{-# LANGUAGE FlexibleContexts #-}
module Crawl.Locations where

import Control.Monad (forM)
import Control.Monad.Error (MonadError, MonadIO)
import qualified Data.Map as Map

import qualified Crawl.Packages as Pkg
import qualified Crawl.Validate as Validate
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution


type Locations =
    Map.Map Module.Name Location

data Location
    = Local FilePath
    | Package N.Name


-- CREATION

initialize :: (MonadIO m, MonadError String m) => [(Module.Name, FilePath)] -> m Locations
initialize locals =
  do  solution <- Solution.read Path.solvedDependencies
      packages <- Pkg.locations (Map.toList solution)

      let rawLocations = Map.toList (combineLocations locals packages)

      validPairs <-
          forM rawLocations $ \(name, allLocations) -> do
              location <- Validate.list locationToString name allLocations
              return (name, location)

      return (Map.fromList validPairs)


combineLocations :: [(Module.Name, FilePath)] -> Pkg.Locations -> Map.Map Module.Name [Location]
combineLocations locals packages =
    Map.unionWith
        (++)
        (Map.fromList (map toLocal locals))
        (Map.map (map Package) packages)
  where
    toLocal (name, filePath) =
        (name, [Local filePath])


locationToString :: Location -> String
locationToString location =
    case location of
      Local path   -> "directory " ++ path 
      Package name -> "package " ++ N.toString name


-- LOOKUP

lookupPath :: Locations -> Module.Name -> Maybe FilePath
lookupPath locations name =
    case Map.lookup name locations of
      Just (Package pkgName) -> error "not sorted out yet"
      Just (Local filePath)  -> Just filePath
      Nothing                -> Nothing
