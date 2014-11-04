{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate where

import Control.Monad.Error (MonadError, MonadIO, forM_, liftIO, throwError)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( dropFileName, takeExtension )
import System.IO ( IOMode(WriteMode), withFile )
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Path
import TheMasterPlan ( ModuleID(ModuleID), Location )


x |> f = f x


generate
    :: (MonadIO m, MonadError String m)
    => FilePath
    -> Map.Map ModuleID [ModuleID]
    -> Map.Map ModuleID Location
    -> [ModuleID]
    -> FilePath
    -> m ()

generate cachePath dependencies natives moduleNames outputFile =
  do  let objectFiles =
            setupNodes cachePath dependencies natives
              |> getReachableObjectFiles moduleNames
      
      liftIO (createDirectoryIfMissing True (dropFileName outputFile))

      case takeExtension outputFile of
        ".html" ->
          case moduleNames of
            [ModuleID moduleName _] ->
              liftIO $
                do  js <- mapM BS.readFile objectFiles
                    BS.writeFile outputFile (html (BS.concat js) moduleName)

            _ ->
              throwError (errorNotOneModule moduleNames)

        _ ->
          liftIO $
          withFile outputFile WriteMode $ \handle ->
              forM_ objectFiles $ \objectFile ->
                  BS.hPut handle =<< BS.readFile objectFile


errorNotOneModule :: [ModuleID] -> String
errorNotOneModule names =
    unlines
    [ "You have specified an HTML output file, so elm-make is attempting to\n"
    , "generate a fullscreen Elm program as HTML. To do this, elm-make must get\n"
    , "exactly one input file, but you have given " ++ show (length names) ++ "."
    ]


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



-- GENERATE HTML

html :: BS.ByteString -> Module.Name -> BS.ByteString
html generatedJavaScript moduleName =
  Blaze.renderMarkup $
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title (H.toHtml (Module.nameToString moduleName))
        H.script ! A.type_ "text/javascript" $
            Blaze.unsafeLazyByteString generatedJavaScript
      H.body $ do
        H.script ! A.type_ "text/javascript" $
            Blaze.preEscapedToMarkup ("Elm.fullscreen(Elm." ++ Module.nameToString moduleName ++ ")")
