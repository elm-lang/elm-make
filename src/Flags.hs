{-# OPTIONS_GHC -Wall #-}
module Flags where

import Control.Applicative ((<$>), (<*>), many, optional)
import Control.Monad.Except (liftIO, throwError)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Elm.Compiler as Compiler
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified BuildManager as BM
import qualified Report


data Flags = Flags
    { _files :: [FilePath]
    , _html :: Maybe FilePath
    , _js :: Maybe FilePath
    , _autoYes :: Bool
    , _reportType :: Report.Type
    , _warn :: Bool
    , _docs :: Maybe FilePath
    }


-- TO CONFIG

toConfig :: BM.Task BM.Config
toConfig =
  do  (Flags files html js autoYes reportType warn docs) <- liftIO parse
      output <- toOutput files html js
      return (BM.Config BM.artifactDirectory files output autoYes reportType warn docs)


toOutput :: [FilePath] -> Maybe FilePath -> Maybe FilePath -> BM.Task BM.Output
toOutput sourceFiles html js =
  case (sourceFiles, html, js) of
    ( [], _, _ ) ->
        throwError BM.BadFlags

    ( _, Just _, Just _ ) ->
        throwError BM.BadFlags

    ( [_], _, Nothing ) ->
        return (BM.Html (maybe "index.html" id html))

    ( _, _, Just outputPath ) ->
        return (BM.JS outputPath)

    ( _ : _ : _, _, Nothing ) ->
        throwError BM.BadFlags


-- PARSE ARGUMENTS

parse :: IO Flags
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo Flags
    parser =
        Opt.info (Opt.helper <*> flags) helpInfo


-- COMMANDS

flags :: Opt.Parser Flags
flags =
    Flags
      <$> files
      <*> optional html
      <*> optional js
      <*> yes
      <*> reportFlag
      <*> warnFlag
      <*> optional docs
  where
    files =
      many (Opt.strArgument ( Opt.metavar "FILES..." ))

    html =
      Opt.strOption $
        mconcat
        [ Opt.long "html"
        , Opt.metavar "FILE"
        , Opt.help "Write resulting HTML to the given FILE."
        ]

    js =
      Opt.strOption $
        mconcat
        [ Opt.long "js"
        , Opt.metavar "FILE"
        , Opt.help "Write resulting JS to the given FILE."
        ]

    docs =
      Opt.strOption $
        mconcat
        [ Opt.long "docs"
        , Opt.metavar "FILE"
        , Opt.help "Write documentation to FILE as JSON."
        ]


-- HELP

helpInfo :: Opt.InfoMod Flags
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
        ++ " (Elm Platform " ++ Compiler.version ++ ")\n"

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-make Main.elm                   # make HTML in index.html"
        , "  elm-make Main.elm --html main.html  # make HTML in main.html"
        , "  elm-make Main.elm --js elm.js       # make JS in elm.js"
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


warnFlag :: Opt.Parser Bool
warnFlag =
    Opt.switch $
        mconcat
        [ Opt.long "warn"
        , Opt.help "Report warnings to improve code quality."
        ]


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
