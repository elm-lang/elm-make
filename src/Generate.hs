{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate where

import Control.Monad.Error (MonadError, MonadIO, forM_, liftIO, throwError)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Tree as Tree
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( dropFileName, takeExtension )
import System.IO ( IOMode(WriteMode), withFile )
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Text as Blaze

import Elm.Utils ((|>))
import qualified Elm.Compiler.Module as Module
import qualified Path
import TheMasterPlan ( ModuleID(ModuleID), Location )


generate
    :: (MonadIO m, MonadError String m)
    => FilePath
    -> Map.Map ModuleID [ModuleID]
    -> Map.Map ModuleID Location
    -> [ModuleID]
    -> FilePath
    -> m ()

generate _cachePath _dependencies _natives [] _outputFile =
  return ()

generate cachePath dependencies natives moduleIDs outputFile =
  do  let objectFiles =
            setupNodes cachePath dependencies natives
              |> getReachableObjectFiles moduleIDs
      
      liftIO (createDirectoryIfMissing True (dropFileName outputFile))

      case takeExtension outputFile of
        ".html" ->
          case moduleIDs of
            [ModuleID moduleName _] ->
              liftIO $
                do  js <- mapM Text.readFile objectFiles
                    Text.writeFile outputFile (html (Text.concat (header:js)) moduleName)

            _ ->
              throwError (errorNotOneModule moduleIDs)

        _ ->
          liftIO $
          withFile outputFile WriteMode $ \handle ->
              do  Text.hPutStrLn handle header
                  forM_ objectFiles $ \jsFile ->
                      Text.hPutStrLn handle =<< Text.readFile jsFile

      liftIO (putStrLn ("Successfully generated " ++ outputFile))


header :: Text.Text
header =
    "var Elm = Elm || { Native: {} };"


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

html :: Text.Text -> Module.Name -> Text.Text
html generatedJavaScript moduleName =
  Blaze.renderMarkup $
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title (H.toHtml (Module.nameToString moduleName))
        H.style $ Blaze.preEscapedToMarkup
            ("html,head,body { padding:0; margin:0; }\n\
             \body { font-family: calibri, helvetica, arial, sans-serif; }" :: Text.Text)
        H.script ! A.type_ "text/javascript" $
            Blaze.preEscapedToMarkup generatedJavaScript
      H.body $ do
        H.script ! A.type_ "text/javascript" $
            Blaze.preEscapedToMarkup ("Elm.fullscreen(Elm." ++ Module.nameToString moduleName ++ ")")
