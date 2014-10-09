module CommandLine.Display where

import qualified Control.Concurrent.Chan as Chan
import System.IO (hFlush, stdout)
import qualified Elm.Compiler.Module as Module


data Update
    = Progress Float
    | Completion Module.Name
    | Done


display :: Chan.Chan Update -> Float -> IO ()
display updates progress =
  do  putStr (renderProgressBar progress)
      hFlush stdout
      update <- Chan.readChan updates
      putStr clearProgressBar
      case update of
        Progress newProgress ->
            display updates newProgress

        Completion name ->
            do  putStrLn $ "Done with " ++ Module.nameToString name
                display updates progress

        Done ->
            putStrLn "Succes!"


-- PROGRESS BAR

barLength :: Float
barLength = 50.0


renderProgressBar :: Float -> String
renderProgressBar fraction =
    "[" ++ replicate numDone '=' ++ replicate numLeft ' ' ++ "] - " ++ show percent ++ "%"
    where
        numDone = truncate (fraction * barLength)
        numLeft = truncate barLength - numDone
        percent = truncate (fraction * 100)


clearProgressBar :: String
clearProgressBar =
    '\r' : replicate (length (renderProgressBar 1)) ' ' ++ "\r"