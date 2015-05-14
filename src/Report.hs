module Report where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (when)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.IO.Handle (hIsTerminalDevice)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package.Name as Pkg
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V
import Elm.Utils ((|>))
import qualified Path
import TheMasterPlan (ModuleID(ModuleID), PackageID)


data Type = Normal | Json


data Message
    = Close
    | Complete ModuleID
    | Error ModuleID FilePath String [Compiler.Error]
    | Warn ModuleID FilePath String [Compiler.Warning]


-- REPORTING THREAD

thread :: Type -> Bool -> Chan.Chan Message -> PackageID -> Int -> IO ()
thread reportType warn messageChan rootPkg totalTasks =
  case reportType of
    Normal ->
        do  isTerminal <- hIsTerminalDevice stdout
            normalLoop isTerminal warn messageChan rootPkg totalTasks 0 0

    Json ->
        jsonLoop messageChan 0


-- JSON LOOP

jsonLoop :: Chan.Chan Message -> Int -> IO ()
jsonLoop messageChan failures =
  do  message <- Chan.readChan messageChan
      case message of
        Close ->
            when (failures > 0) exitFailure

        Complete _moduleID ->
            jsonLoop messageChan failures

        Error _moduleID path _source errors ->
            let
              errorObjects =
                map (Compiler.errorToJson path) errors
            in
              do  BS.putStrLn (Json.encode errorObjects)
                  jsonLoop messageChan (failures + 1)

        Warn _moduleID path _source warnings ->
            let
              warningObjects =
                map (Compiler.warningToJson path) warnings
            in
              do  BS.putStrLn (Json.encode warningObjects)
                  jsonLoop messageChan failures


-- NORMAL LOOP

normalLoop :: Bool -> Bool -> Chan.Chan Message -> PackageID -> Int -> Int -> Int -> IO ()
normalLoop isTerminal warn messageChan rootPkg total successes failures =
  let go = normalLoop isTerminal warn messageChan rootPkg total
  in
  do  when isTerminal $
          do  hPutStr stdout (renderProgressBar successes failures total)
              hFlush stdout

      update <- Chan.readChan messageChan

      when isTerminal $
          hPutStr stdout clearProgressBar

      case update of
        Complete _moduleID ->
            go (successes + 1) failures

        Close ->
            do  hPutStrLn stdout (closeMessage failures total)
                when (failures > 0) exitFailure

        Error (ModuleID _ pkg) path source errors ->
            do  hFlush stdout

                errors
                  |> concatMap (Compiler.errorToString path source)
                  |> errorMessage rootPkg pkg path
                  |> hPutStr stderr

                go successes (failures + 1)

        Warn (ModuleID _ pkg) path source warnings ->
          if not warn
            then go successes failures
            else
            do  hFlush stdout

                let maybeMessage =
                      warnings
                        |> concatMap (Compiler.warningToString path source)
                        |> warningMessage rootPkg pkg path

                maybe (return ()) (hPutStr stderr) maybeMessage

                go successes failures


-- ERROR MESSAGE

errorMessage :: PackageID -> PackageID -> FilePath -> String -> String
errorMessage rootPkg errorPkg path msg =
  if errorPkg /= rootPkg
    then dependencyError errorPkg
    else header "ERRORS" path ++ msg


dependencyError :: PackageID -> String
dependencyError (pkgName, version) =
    header "ERRORS" ("dependency " ++ Pkg.toString pkgName ++ " " ++ V.toString version)
    ++ "This error probably means that the '" ++ Pkg.toString pkgName ++ "' has some\n"
    ++ "a package constraint that is too permissive. You should definitely inform the\n"
    ++ "maintainer to get this fixed and save other people from this pain.\n\n"
    ++ "In the meantime, you can attempt to artificially constrain things by adding\n"
    ++ "some extra constraints to your " ++ Path.description ++ " though that is not\n"
    ++ "the long term solution.\n\n\n"


-- WARNING MESSAGE

warningMessage :: PackageID -> PackageID -> FilePath -> String -> Maybe String
warningMessage rootPkg warningPkg path msg =
  if warningPkg /= rootPkg
    then Nothing
    else Just (header "WARNINGS" path ++ msg)


header :: String -> FilePath -> String
header title path =
  let
    start = "## " ++ title ++ " in " ++ path
  in
    start ++ replicate (80 - length start) '#' ++ "\n\n"


-- PROGRESS BAR

barLength :: Float
barLength = 50.0


renderProgressBar :: Int -> Int -> Int -> String
renderProgressBar successes failures total =
    "[" ++ replicate numDone '=' ++ replicate numLeft ' ' ++ "] - "
    ++ show (successes + failures) ++ " / " ++  show total
  where
    fraction = fromIntegral (successes + failures) / fromIntegral total
    numDone = truncate (fraction * barLength)
    numLeft = truncate barLength - numDone


clearProgressBar :: String
clearProgressBar =
    '\r' : replicate (length (renderProgressBar 49999 50000 99999)) ' ' ++ "\r"


-- CLOSE MESSAGE

closeMessage :: Int -> Int -> String
closeMessage failures total =
  case failures of
    0 -> "Success! Compiled " ++ show total ++ " modules."
    1 -> "Detected errors in 1 module."
    n -> "Detected errors in " ++ show n ++ " modules."
