module Options where

import Control.Applicative ((<$>), (<*>), (<|>), pure, many)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Option = [FilePath]


parse :: IO Option
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo Option
    parser =
        Opt.info (Opt.helper <*> option) helpInfo


-- COMMANDS

option :: Opt.Parser Option
option =
    many $ Opt.strArgument ( Opt.metavar "FILES..." )


-- HELP

helpInfo :: Opt.InfoMod Option
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
