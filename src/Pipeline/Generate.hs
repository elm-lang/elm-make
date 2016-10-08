{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Pipeline.Generate where

import Control.Monad.Except (forM_, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Tree as Tree
import Elm.Utils ((|>))
import qualified Elm.Compiler as Elm
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( dropFileName )
import System.IO ( IOMode(WriteMode) )
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Text as Blaze
import Text.RawString.QQ (r)

import qualified BuildManager as BM
import qualified Path
import qualified TheMasterPlan as TMP
import qualified Utils.File as File



-- GENERATE DOCS


docs :: [Docs.Documentation] -> FilePath -> BM.Task ()
docs docsList path =
  Docs.prettyJson docsList
    |> LazyText.decodeUtf8
    |> LazyText.replace "\\u003e" ">"
    |> LazyText.writeFile path
    |> liftIO



-- GENERATE ELM STUFF


generate
    :: BM.Config
    -> Map.Map TMP.CanonicalModule Module.Interface
    -> Map.Map TMP.CanonicalModule [TMP.CanonicalModule]
    -> Map.Map TMP.CanonicalModule TMP.Location
    -> [TMP.CanonicalModule]
    -> BM.Task ()

generate _config _interfaces _dependencies _natives [] =
  return ()

generate config interfaces dependencies natives rootModules =
  do  let objectFiles =
            setupNodes (BM._artifactDirectory config) dependencies natives
              |> getReachableObjectFiles (BM._debug config) rootModules

      let outputFile = BM.outputFilePath config
      liftIO (createDirectoryIfMissing True (dropFileName outputFile))

      let footer = createFooter (BM._debug config) interfaces rootModules

      case BM._output config of
        BM.Html outputFile ->
          liftIO $
            do  js <- mapM File.readTextUtf8 objectFiles
                let (TMP.CanonicalModule _ moduleName) = head rootModules
                let outputText = html (Text.concat (header : js ++ [footer])) moduleName
                LazyText.writeFile outputFile outputText

        BM.JS outputFile ->
          liftIO $
          File.withFileUtf8 outputFile WriteMode $ \handle ->
              do  Text.hPutStrLn handle header
                  forM_ objectFiles $ \jsFile ->
                      Text.hPutStrLn handle =<< File.readTextUtf8 jsFile
                  Text.hPutStrLn handle footer

        BM.DevNull ->
          return ()

      liftIO (putStrLn ("Successfully generated " ++ outputFile))


setupNodes
    :: FilePath
    -> Map.Map TMP.CanonicalModule [TMP.CanonicalModule]
    -> Map.Map TMP.CanonicalModule TMP.Location
    -> [(FilePath, TMP.CanonicalModule, [TMP.CanonicalModule])]
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
  :: Bool
  -> [TMP.CanonicalModule]
  -> [(FilePath, TMP.CanonicalModule, [TMP.CanonicalModule])]
  -> [FilePath]
getReachableObjectFiles debug moduleNames allNodes =
  let
    nodes =
      if debug then allNodes else filter (not . isVirtualDomDebug) allNodes

    (dependencyGraph, vertexToKey, keyToVertex) =
      Graph.graphFromEdges nodes

    reachableSet =
      Maybe.mapMaybe keyToVertex moduleNames
        |> Graph.dfs dependencyGraph
        |> concatMap Tree.flatten
        |> Set.fromList
  in
    Graph.topSort dependencyGraph
      |> filter (\vtx -> Set.member vtx reachableSet)
      |> reverse
      |> map vertexToKey
      |> map (\(path, _, _) -> path)


isVirtualDomDebug :: (fp, TMP.CanonicalModule, deps) -> Bool
isVirtualDomDebug (_filePath, TMP.CanonicalModule (pkg, _vsn) name, _deps) =
  pkg == Pkg.virtualDom && name == ["VirtualDom","Debug"]



-- GENERATE HTML


html :: Text.Text -> Module.Raw -> LazyText.Text
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
            Blaze.preEscapedToMarkup ("Elm." ++ Module.nameToString moduleName ++ ".fullscreen()")



-- FOOTER


createFooter
  :: Bool
  -> Map.Map TMP.CanonicalModule Module.Interface
  -> [TMP.CanonicalModule]
  -> Text.Text
createFooter debugMode canonicalInterfaces rootModules =
  let
    interfaces =
      Map.mapKeys TMP.simplifyModuleName canonicalInterfaces

    exportChunks =
      map
        (exportProgram debugMode interfaces)
        (List.sort (map TMP.simplifyModuleName rootModules))
  in
    Text.pack $
      "var Elm = {};\n"
      ++ unlines exportChunks
      ++ footerClose


exportProgram
  :: Bool
  -> Map.Map Module.Canonical Module.Interface
  -> Module.Canonical
  -> String
exportProgram debugMode interfaces canonicalName@(Module.Canonical _ moduleName) =
  let
    program =
      Module.qualifiedVar canonicalName "main"

    object =
      objectFor moduleName

    name =
      Module.nameToString moduleName

    debugArg =
      if debugMode then createDebugMetadata interfaces canonicalName else "undefined"
  in
    setup moduleName
    ++ "if (typeof " ++ program ++ " !== 'undefined') {\n    "
    ++ program ++ "(" ++ object ++ ", '" ++ name ++ "', " ++ debugArg ++ ");\n}"


createDebugMetadata :: Map.Map Module.Canonical Module.Interface -> Module.Canonical -> String
createDebugMetadata interfaces canonicalName =
  let
    metadataFields =
      [ "versions" .= Json.object [ "elm" .= Elm.version ]
      , "types" .= Module.programTypes interfaces canonicalName
      ]
  in
    Json.object metadataFields
      |> Json.encode
      |> LazyText.decodeUtf8
      |> LazyText.replace "\\u003e" ">"
      |> LazyText.unpack


setup :: [String] -> String
setup moduleName =
  let
    create path =
      let
        jsPath =
          objectFor path
      in
        jsPath ++ " = " ++ jsPath ++ " || {};"

    paths =
      tail (List.inits moduleName)
  in
    unlines (map create paths)


objectFor :: Module.Raw -> String
objectFor names =
  let
    brackets :: String -> String
    brackets name =
      "['" ++ name ++ "']"
  in
    "Elm" ++ concatMap brackets names


footerClose :: String
footerClose = [r|
if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);
|]



-- HEADER


header :: Text.Text
header = [r|
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
|]
