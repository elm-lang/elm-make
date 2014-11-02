module Generate where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import System.IO ( IOMode(WriteMode), withFile )

import qualified Path
import TheMasterPlan (ModuleID, Location)


x |> f = f x


js  :: FilePath
    -> Map.Map ModuleID [ModuleID]
    -> Map.Map ModuleID Location
    -> [ModuleID]
    -> FilePath
    -> IO ()

js cachePath dependencies natives moduleNames outputFile =
    let objectFiles =
            setupNodes cachePath dependencies natives
              |> getReachableObjectFiles moduleNames
    in
        withFile outputFile WriteMode $ \handle ->
            forM_ objectFiles $ \objectFile ->
                BS.hPut handle =<< BS.readFile objectFile



setupNodes
    :: FilePath
    -> Map.Map ModuleID [ModuleID]
    -> Map.Map ModuleID Location
    -> [(FilePath, ModuleID, [ModuleID])]
setupNodes cachePath dependencies natives =
    let nativeNodes =
            Map.toList natives
              |> map (\(name, loc) -> (Path.toSource loc, name, []))

        dependencyNodes =
            Map.toList dependencies
              |> map (\(name, deps) -> (Path.toObjectFile cachePath name, name, deps))
    in
        nativeNodes ++ dependencyNodes


getReachableObjectFiles
    :: [ModuleID]
    -> [(FilePath, ModuleID, [ModuleID])]
    -> [FilePath]
getReachableObjectFiles moduleNames nodes =
    let (dependencyGraph, vertexToKey, keyToVertex) =
            Graph.graphFromEdges nodes
    in
        Maybe.mapMaybe keyToVertex moduleNames
          |> Graph.dfs dependencyGraph
          |> concatMap Tree.flatten
          |> Set.fromList
          |> Set.toList
          |> map vertexToKey
          |> map (\(path, _, _) -> path)
