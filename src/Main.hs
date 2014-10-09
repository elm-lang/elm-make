module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)

import Crawl


main :: IO ()
main =
    do  numProcessors <- getNumProcessors
        setNumCapabilities numProcessors
        undefined

overarchingThing =
    do  (locations, dependencyNodes) <- Crawl.crawl
        return ()

