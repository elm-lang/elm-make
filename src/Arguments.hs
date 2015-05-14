module Arguments where

import Control.Applicative ((<$>), (<*>), many, optional)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Elm.Compiler as Compiler
import qualified Report


data Arguments = Arguments
    { files :: [FilePath]
    , outputFile :: Maybe FilePath
    , autoYes :: Bool
    , reportType :: Report.Type
    , warn :: Bool
    }


-- PARSE ARGUMENTS

parse :: IO Arguments
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo Arguments
    parser =
        Opt.info (Opt.helper <*> options) helpInfo


-- COMMANDS

options :: Opt.Parser Arguments
options =
    Arguments
      <$> files
      <*> optional outputFile
      <*> yes
      <*> reportFlag
      <*> warnFlag
  where
    files =
      many (Opt.strArgument ( Opt.metavar "FILES..." ))

    outputFile =
      Opt.strOption $
        mconcat
        [ Opt.long "output"
        , Opt.metavar "FILE"
        , Opt.help "Write output to FILE."
        ]


-- HELP

helpInfo :: Opt.InfoMod Arguments
helpInfo =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "build Elm projects"
        , Opt.footerDoc (Just moreHelp)
        ]
  where
    top =
        "elm-make " ++ showVersion This.version
        ++ " (Elm Platform " ++ Compiler.version ++ ")\n"

    moreHelp =
        linesToDoc
        [ "To learn more about a particular command run:"
        , "    elm-make COMMAND --help"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lines =
    PP.vcat (map PP.text lines)


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
