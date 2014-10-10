module Main where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import GHC.Conc (getNumProcessors, setNumCapabilities)

import qualified Build
import qualified Crawl
import qualified Options
import qualified Build.Display as Display
import qualified Elm.Compiler.Module as Module


main :: IO ()
main =
    do  numProcessors <- getNumProcessors
        setNumCapabilities numProcessors
        updates <- Chan.newChan
        forkIO $ Display.display updates 0
        scriptSequence updates updateSequence


updateSequence =
    [ Display.Progress 0.1
    , Display.Progress 0.3
    , Display.Completion (Module.Name ["Focus"])
    , Display.Completion (Module.Name ["CommandLine", "Display"])
    , Display.Progress 0.5
    , Display.Progress 1
    , Display.Success
    ]


scriptSequence :: Chan.Chan Display.Update -> [Display.Update] -> IO ()
scriptSequence _updateChan [] = return ()
scriptSequence updateChan (update:rest) =
    do  Chan.writeChan updateChan update
        threadDelay (1000 * 1000)
        scriptSequence updateChan rest

{--
overarchingThing =
    do  (locations, dependencyNodes) <- Crawl.crawl
        return ()
--}
