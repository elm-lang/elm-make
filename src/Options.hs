module Options where

import Control.Applicative ((<$>), (<*>), (<|>), pure, many, optional)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Options = Options
    { files :: [FilePath]
    , outputFile :: Maybe FilePath
    }


parse :: IO Options
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo Options
    parser =
        Opt.info (Opt.helper <*> options) helpInfo


-- COMMANDS

options :: Opt.Parser Options
options =
    Options
      <$> files
      <*> optional outputFile
  where
    files =
      many (Opt.strArgument ( Opt.metavar "FILES..." ))

    outputFile =
      Opt.strOption
         ( Opt.long "output"
        <> Opt.short 'o'
        <> Opt.metavar "FILE"
        <> Opt.help "Write output to FILE." )


-- HELP

helpInfo :: Opt.InfoMod Options
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
        ++ ", (c) Evan Czaplicki 2014\n"

    moreHelp =
        linesToDoc
        [ "To learn more about a particular command run:"
        , "    elm-package COMMAND --help"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lines =
    PP.vcat (map PP.text lines)
