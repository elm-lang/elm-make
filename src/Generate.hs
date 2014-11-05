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
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
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
  do  coreModuleIDs <- getCoreModules natives

      let objectFiles =
            setupNodes cachePath dependencies natives
              |> getReachableObjectFiles (coreModuleIDs ++ moduleIDs)
      
      runtimePath <- liftIO Compiler.runtimePath

      let allFiles = runtimePath : objectFiles

      liftIO (createDirectoryIfMissing True (dropFileName outputFile))

      case takeExtension outputFile of
        ".html" ->
          case moduleIDs of
            [ModuleID moduleName _] ->
              liftIO $
                do  js <- mapM Text.readFile allFiles
                    Text.writeFile outputFile (html (Text.concat js) moduleName)

            _ ->
              throwError (errorNotOneModule moduleIDs)

        _ ->
          liftIO $
          withFile outputFile WriteMode $ \handle ->
              forM_ allFiles $ \jsFile ->
                  Text.hPutStr handle =<< Text.readFile jsFile


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


getCoreModules
    :: (MonadError String m)
    => Map.Map ModuleID Location
    -> m [ModuleID]
getCoreModules natives =
  case (length foundCores, length requiredCores) of
    (found, required)
      | found < required -> throwError (show found ++ " < " ++ show required ++ "\n" ++ show requiredCores)
      | found == required -> return foundCores
      | found > required -> throwError "Something went badly wrong.\nAre you somehow using two versions of elm-lang/core?"
  where
    foundCores =
      Maybe.mapMaybe isCore (Map.keys natives)

    isCore moduleID@(ModuleID (Module.Name name) pkg) =
      case pkg of
        Just (Pkg.Name "elm-lang" "core", _)
          | name `elem` requiredCores ->
              Just moduleID

        _ -> Nothing


requiredCores =
  [ ["Native","Utils"]
  , ["Native","Signal"]
  ]


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
