module Report where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (when)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import Elm.Utils ((|>))
import GHC.IO.Handle (hIsTerminalDevice)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import TheMasterPlan (CanonicalModule(CanonicalModule), Package)


data Type = Normal | Json


data Message
    = Close
    | Complete CanonicalModule
    | Error CanonicalModule Compiler.Dealiaser FilePath String [Compiler.Error]
    | Warn CanonicalModule Compiler.Dealiaser FilePath String [Compiler.Warning]


-- REPORTING THREAD

thread :: Type -> Bool -> Chan.Chan Message -> Package -> Int -> IO ()
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

        Error _moduleID dealiaser path _source errors ->
            let
              errorObjects =
                map (Compiler.errorToJson dealiaser path) errors
            in
              do  BS.putStrLn (Json.encode errorObjects)
                  jsonLoop messageChan (failures + 1)

        Warn _moduleID dealiaser path _source warnings ->
            let
              warningObjects =
                map (Compiler.warningToJson dealiaser path) warnings
            in
              do  BS.putStrLn (Json.encode warningObjects)
                  jsonLoop messageChan failures


-- NORMAL LOOP

normalLoop :: Bool -> Bool -> Chan.Chan Message -> Package -> Int -> Int -> Int -> IO ()
normalLoop isTerminal warn messageChan rootPkg total successes failures =
  let
    go =
      normalLoop isTerminal warn messageChan rootPkg total

    put withColor withoutColor dealiaser path source errors =
      if isTerminal then
        withColor dealiaser path source errors
      else
        hPutStr stderr (withoutColor dealiaser path source errors)

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

        Error (CanonicalModule pkg _) dealiaser path source errors ->
            do  hFlush stdout

                errors
                  |> mapM_ (put Compiler.printError Compiler.errorToString dealiaser path source)
                  |> errorMessage rootPkg pkg path

                go successes (failures + 1)

        Warn (CanonicalModule pkg _) dealiaser path source warnings ->
            if not warn then
              go successes failures
            else
              do  hFlush stdout

                  warnings
                    |> mapM_ (put Compiler.printWarning Compiler.warningToString dealiaser path source)
                    |> warningMessage rootPkg pkg path

                  go successes failures


-- ERROR MESSAGE

errorMessage :: Package -> Package -> FilePath -> IO () -> IO ()
errorMessage rootPkg errorPkg path printMessage =
  if errorPkg /= rootPkg then
      hPutStr stderr (dependencyError errorPkg)

  else
      do  hPutStr stderr (header "ERRORS" path)
          printMessage


dependencyError :: Package -> String
dependencyError (pkgName, version) =
    header "ERRORS" ("dependency " ++ Pkg.toString pkgName ++ " " ++ Pkg.versionToString version)
    ++ "This error probably means that the '" ++ Pkg.toString pkgName ++ "' has some\n"
    ++ "package constraint that is too permissive. You should definitely inform the\n"
    ++ "maintainer to get this fixed and save other people from this pain.\n\n"
    ++ "In the meantime, you can attempt to artificially constrain things by adding\n"
    ++ "some extra constraints to your " ++ Path.description ++ " though that is not\n"
    ++ "the long term solution.\n\n\n"


-- WARNING MESSAGE

warningMessage :: Package -> Package -> FilePath -> IO () -> IO ()
warningMessage rootPkg warningPkg path printMessage =
  if warningPkg /= rootPkg
    then return ()
    else
      do  hPutStr stderr (header "WARNINGS" path)
          printMessage


header :: String -> FilePath -> String
header title path =
  let
    start = "## " ++ title ++ " in " ++ path ++ " "
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
