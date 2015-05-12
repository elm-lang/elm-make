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

        Error (ModuleID _name pkg) location msg ->
            do  putStrLn ""
                hPutStr stderr (errorMessage rootPkg pkg location msg)
                exitFailure


-- ERROR MESSAGE

errorMessage :: PackageID -> PackageID -> Location -> String -> String
errorMessage rootPkg errorPkg location msg =
  if errorPkg /= rootPkg
    then
      dependencyError errorPkg
    else
      let
        start = "## ERRORS "
        end = " " ++ Path.toSource location
      in
        start ++ replicate (80 - length start - length end) '#' ++ end ++ "\n\n" ++ msg


dependencyError :: PackageID -> String
dependencyError (pkgName, version) =
  let
    start =
      "## ERROR in dependency " ++ Pkg.toString pkgName
      ++ " " ++ V.toString version ++ " "
  in
    start ++ replicate (80 - length start) '#' ++ "\n\n"
    ++ "This error probably means that the '" ++ Pkg.toString pkgName ++ "' has some\n"
    ++ "a package constraint that is too permissive. You should definitely inform the\n"
    ++ "maintainer to get this fixed and save other people from this pain.\n\n"
    ++ "In the meantime, you can attempt to artificially constrain things by adding\n"
    ++ "some extra constraints to your " ++ Path.description ++ " though that is not\n"
    ++ "the long term solution.\n\n\n"


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