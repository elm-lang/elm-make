{-# OPTIONS_GHC -Wall #-}
module Flags where

import Control.Applicative (many, optional)
import Control.Monad.Except (liftIO)
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Version (showVersion)
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import System.FilePath (takeExtension)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified BuildManager as BM
import qualified Report



-- TO CONFIG


toConfig :: BM.Task BM.Config
toConfig =
  liftIO parse



-- PARSE ARGUMENTS


parse :: IO BM.Config
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo BM.Config
    parser =
        Opt.info (Opt.helper <*> flags) helpInfo



-- COMMANDS


flags :: Opt.Parser BM.Config
flags =
    BM.Config BM.artifactDirectory
      <$> files
      <*> output
      <*> yes
      <*> reportFlag
      <*> debugFlag
      <*> warnFlag
      <*> optional docs
      <*> capabilities
  where
    files =
      many (Opt.strArgument ( Opt.metavar "FILES..." ))

    docs =
      Opt.strOption $
        mconcat
        [ Opt.long "docs"
        , Opt.metavar "FILE"
        , Opt.help "Write documentation to FILE as JSON."
        ]

    capabilities =
      setCapabilities
        <$> Opt.switch (Opt.long "prepublish")
        <*> Opt.switch (Opt.long "prepublish-core")


setCapabilities :: Bool -> Bool -> BM.Permissions
setCapabilities isNormal isCore =
  if isCore then
    BM.Effects

  else if isNormal then
    BM.None

  else
    BM.PortsAndEffects



-- HELP


helpInfo :: Opt.InfoMod BM.Config
helpInfo =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "build Elm projects"
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        "elm-make " ++ showVersion This.version
        ++ " (Elm Platform " ++ (Pkg.versionToString Compiler.version) ++ ")\n"

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-make Main.elm                     # compile to HTML in index.html"
        , "  elm-make Main.elm --output main.html  # compile to HTML in main.html"
        , "  elm-make Main.elm --output elm.js     # compile to JS in elm.js"
        , "  elm-make Main.elm --warn              # compile and report warnings"
        , ""
        , "Full guide to using elm-make at <https://github.com/elm-lang/elm-make>"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)


yes :: Opt.Parser Bool
yes =
    Opt.switch $
        mconcat
        [ Opt.long "yes"
        , Opt.help "Reply 'yes' to all automated prompts."
        ]


debugFlag :: Opt.Parser Bool
debugFlag =
    Opt.switch $
        mconcat
        [ Opt.long "debug"
        , Opt.help "Generate programs in debug mode."
        ]


warnFlag :: Opt.Parser Bool
warnFlag =
    Opt.switch $
        mconcat
        [ Opt.long "warn"
        , Opt.help "Report warnings to improve code quality."
        ]


output :: Opt.Parser BM.Output
output =
  let
    fromString path =
      let
        ext =
          takeExtension path
      in
        if ext == ".html" then
          Just (BM.Html path)

        else if ext == ".js" then
          Just (BM.JS path)

        else if path == "/dev/null" then
          Just BM.DevNull

        else
          Nothing

    rawFlag =
      mconcat
        [ Opt.long "output"
        , Opt.metavar "FILE"
        , Opt.value (BM.Html "index.html")
        , Opt.help "Write result to the given .html or .js FILE."
        ]

    validInput =
      ["any file that ends with .html or .js"]
  in
    Opt.option (argReader validInput fromString) rawFlag


reportFlag :: Opt.Parser Report.Type
reportFlag =
  let
    fromString str =
      case str of
        "normal" -> Just Report.Normal
        "json" -> Just Report.Json
        _ -> Nothing

    options =
      mconcat
        [ Opt.long "report"
        , Opt.metavar "FORMAT"
        , Opt.value Report.Normal
        , Opt.help "Format of error and warning reports (e.g. --report=json)"
        ]
  in
    Opt.option (argReader ["normal","json"] fromString) options


argReader :: [String] -> (String -> Maybe a) -> Opt.ReadM a
argReader options fromString =
  let reader arg =
        case fromString arg of
          Just a ->
              Right a

          Nothing ->
              Left $
                "acceptable arguments to this flag include: "
                ++ List.intercalate ", " options
  in
      Opt.eitherReader reader
