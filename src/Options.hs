module Options where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Option
    = BuildFile FilePath
    | BuildPackage


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
    buildFile
    <|> pure BuildPackage

buildFile :: Opt.Parser Option
buildFile =
  BuildFile <$>
    Opt.strArgument
        (  Opt.metavar "FILE"
        <> Opt.help "an Elm file that you want to compile"
        )


-- HELP

helpInfo :: Opt.InfoMod Option
helpInfo =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "install and publish elm libraries"
        , Opt.footerDoc (Just moreHelp)
        ]
  where
    top =
        "Elm Package Manager " ++ showVersion This.version
        ++ ", (c) Evan Czaplicki 2013-2014\n"

    moreHelp =
        linesToDoc
        [ "To learn more about a particular command run:"
        , "    elm-package COMMAND --help"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lines =
    PP.vcat (map PP.text lines)
