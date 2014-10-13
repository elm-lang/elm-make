module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)

--import qualified Build
import qualified CrawlPackage
import qualified CrawlProject
import qualified LoadInterfaces
import qualified Options
import qualified Elm.Compiler.Module as Module


main :: IO ()
main =
  do  numProcessors <- getNumProcessors
      setNumCapabilities numProcessors

      option <- Options.parse
      case option of
        Options.BuildPackage -> return ()
        Options.BuildFile path -> return ()
