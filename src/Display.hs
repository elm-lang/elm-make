module Display where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import GHC.IO.Handle (hIsTerminalDevice)
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V
import qualified Path
import TheMasterPlan (Location, ModuleID(ModuleID), PackageID)


data Update
    = Completion ModuleID
    | Success
    | Error ModuleID Location String


display :: Chan.Chan Update -> PackageID -> Int -> Int -> IO ()
display updatesChan rootPkg completeTasks totalTasks =
  do  isTerminal <- hIsTerminalDevice stdout
      loop isTerminal updatesChan rootPkg completeTasks totalTasks


loop :: Bool -> Chan.Chan Update -> PackageID -> Int -> Int -> IO ()
loop isTerminal updatesChan rootPkg completeTasks totalTasks =
  do  when isTerminal $
          do  hPutStr stdout (renderProgressBar completeTasks totalTasks)
              hFlush stdout

      update <- Chan.readChan updatesChan
      
      when isTerminal $
          hPutStr stdout clearProgressBar

      case update of
        Completion _moduleID ->
            loop isTerminal updatesChan rootPkg (completeTasks + 1) totalTasks

        Success ->
            case completeTasks of
              0 -> return ()
              1 -> putStrLn $ "Compiled 1 file"
              _ -> putStrLn $ "Compiled " ++ show completeTasks ++ " files"

        Error (ModuleID name pkg) location msg ->
            do  putStrLn ""
                hPutStrLn stderr (errorMessage rootPkg pkg name location msg)
                exitFailure


-- ERROR MESSAGE

errorMessage :: PackageID -> PackageID -> Module.Name -> Location -> String -> String
errorMessage rootPkg errorPkg@(pkgName, version) name location msg =
    "Error" ++ context ++ ":\n\n" ++ msg ++ report
  where
    isLocalError = errorPkg == rootPkg

    context
      | isLocalError = " in " ++ Path.toSource location
      | otherwise =
          " in package " ++ Pkg.toString pkgName ++ " " ++ V.toString version
          ++ " in module " ++ Module.nameToString name

    report
      | isLocalError = ""
      | otherwise =
          "\n\nThis error is probably due to bad version bounds. You should definitely\n"
          ++ "inform the maintainer of " ++ Pkg.toString pkgName ++ " to get this fixed.\n\n"
          ++ "In the meantime, you can attempt to get rid of the problematic dependency by\n"
          ++ "modifying " ++ Path.solvedDependencies ++ ", though that is not a long term\n"
          ++ "solution."


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


clearProgressBar :: String
clearProgressBar =
    '\r' : replicate (length (renderProgressBar 99999 99999)) ' ' ++ "\r"