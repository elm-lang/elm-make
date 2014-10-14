module Build.Display where

import qualified Control.Concurrent.Chan as Chan
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified Elm.Compiler.Module as Module
import TheMasterPlan (ModuleID(ModuleID))


data Update
    = Completion ModuleID
    | Success
    | Error String


display :: Chan.Chan Update -> Int -> Int -> IO ()
display updates completeTasks totalTasks =
  do  putStr (renderProgressBar completeTasks totalTasks)
      hFlush stdout
      update <- Chan.readChan updates
      putStr clearProgressBar
      case update of
        Completion (ModuleID name _pkg) ->
            do  putStrLn $ "Done with " ++ Module.nameToString name
                display updates (completeTasks + 1) totalTasks

        Success ->
            putStrLn $ "Success! Compiled " ++ show completeTasks ++ " files."

        Error msg ->
            do  hPutStrLn stderr ("Error: " ++ msg)
                exitFailure


-- PROGRESS BAR

barLength :: Float
barLength = 50.0


renderProgressBar :: Int -> Int -> String
renderProgressBar complete total =
    "[" ++ replicate numDone '=' ++ replicate numLeft ' ' ++ "] - " ++ show complete ++ " / " ++ show total
    where
        fraction = fromIntegral complete / fromIntegral total
        numDone = truncate (fraction * barLength)
        numLeft = truncate barLength - numDone
        percent = truncate (fraction * 100)


clearProgressBar :: String
clearProgressBar =
    '\r' : replicate (length (renderProgressBar 99999 99999)) ' ' ++ "\r"