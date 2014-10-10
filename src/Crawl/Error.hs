module Crawl.Error where

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Paths as Path


notFound :: Module.Name -> String
notFound name =
    unlines
    [ "could not find module '" ++ Module.nameToString name ++ "'"
    , ""
    , "Potential problems could be:"
    , "  * Misspelled the module name"
    , "  * Need to add a source directory or new dependency to " ++ Path.description
    ]


tooMany :: Module.Name -> [String] -> String
tooMany name searchedList =
    "found multiple modules named '" ++ Module.nameToString name ++ "'\n"
    ++ "Modules with that name were found in the following locations:\n\n"
    ++ concatMap (\str -> "    " ++ str ++ "\n") searchedList


nameMismatch :: FilePath -> Module.Name -> Module.Name -> String
nameMismatch path nameFromPath nameFromSource =
    unlines
    [ "The module name is messed up for " ++ path
    , "    According to the file's name it should be " ++ Module.nameToString nameFromPath
    , "    According to the source code it should be " ++ Module.nameToString nameFromSource
    , "Which is it?"
    ]
