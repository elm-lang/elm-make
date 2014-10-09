{-# LANGUAGE FlexibleContexts #-}
module Crawl.Validate where

import Control.Monad.Error (MonadError, throwError)
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Paths as Path


list :: (MonadError String m) => (a -> String) -> Module.Name -> [a] -> m a
list display name values =
    case values of
      [value] -> return value
      [] -> throwError (reportNotFound name)
      _  -> throwError (reportTooMany name (map display values))


reportNotFound :: Module.Name -> String
reportNotFound name =
    unlines
    [ "could not find module '" ++ Module.nameToString name ++ "'"
    , ""
    , "Potential problems could be:"
    , "  * Misspelled the module name"
    , "  * Need to add a source directory or new dependency to " ++ Path.description
    ]


reportTooMany :: Module.Name -> [String] -> String
reportTooMany name searchedList =
    "found multiple modules named '" ++ Module.nameToString name ++ "'\n"
    ++ "Modules with that name were found in the following locations:\n\n"
    ++ concatMap (\str -> "    " ++ str ++ "\n") searchedList
