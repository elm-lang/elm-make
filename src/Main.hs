{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (foldM)
import Control.Monad.Error (MonadError, throwError, ErrorT, MonadIO, liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V

import Crawl


main :: IO ()
main =
    undefined

