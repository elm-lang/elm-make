module Display where

import qualified Control.Concurrent.Chan as Chan
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Version as V
import TheMasterPlan (ModuleID(ModuleID), PackageID)


data Update
    = Completion ModuleID
    | Success
    | Error ModuleID String


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

        Error (ModuleID name pkg) msg ->
            do  putStrLn ""
                hPutStrLn stderr (errorMessage name pkg msg)
                exitFailure


-- ERROR MESSAGE

errorMessage :: Module.Name -> Maybe PackageID -> String -> String
errorMessage name maybePackage msg =
    "Error when compiling " ++ Module.nameToString name ++ " module"
    ++ context ++ ":\n" ++ msg
  where
    context =
        case maybePackage of
          Nothing -> ""
          Just (pkgName, version) ->
              " in package " ++ Pkg.toString pkgName ++ " " ++ V.toString version


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