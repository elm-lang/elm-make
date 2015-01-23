module Arguments where

import Control.Applicative ((<$>), (<*>), many, optional)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_make as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Elm.Compiler as Compiler


data Arguments = Arguments
    { files :: [FilePath]
    , outputFile :: Maybe FilePath
    , autoYes :: Bool
    }


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
