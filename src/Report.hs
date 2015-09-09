module Report where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (when)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import GHC.IO.Handle (hIsTerminalDevice)
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import TheMasterPlan (CanonicalModule(CanonicalModule), Package)


data Type = Normal | Json


data Message
    = Close
    | Complete CanonicalModule Compiler.Localizer FilePath String [Compiler.Warning]
    | Error CanonicalModule Compiler.Localizer FilePath String [Compiler.Warning] [Compiler.Error]



-- REPORTING THREAD


thread :: Type -> Bool -> Chan.Chan Message -> Package -> Int -> IO ()
thread reportType warn messageChan rootPkg totalTasks =
  case reportType of
    Normal ->
        do  isTerminal <- checkIsTerminal
            normalLoop isTerminal warn messageChan rootPkg totalTasks 0 0

    Json ->
        jsonLoop messageChan rootPkg 0



-- JSON LOOP


jsonLoop :: Chan.Chan Message -> Package -> Int -> IO ()
jsonLoop messageChan rootPkg failures =
  do  message <- Chan.readChan messageChan
      case message of
        Close ->
            when (failures > 0) exitFailure

        Complete (CanonicalModule pkg _) localizer path _ warnings ->
            do  when (pkg == rootPkg) $
                  printJsonList (Compiler.warningToJson localizer path) warnings
                jsonLoop messageChan rootPkg failures

        Error (CanonicalModule pkg _) localizer path _source warnings errors ->
            do  when (pkg == rootPkg) $
                  printJsonList (Compiler.warningToJson localizer path) warnings
                printJsonList (Compiler.errorToJson localizer path) errors
                jsonLoop messageChan rootPkg (failures + 1)


printJsonList :: (a -> Json.Value) -> [a] -> IO ()
printJsonList toJson values =
  case values of
    [] ->
      return ()

    _ ->
      BS.putStrLn (Json.encode (map toJson values))



-- NORMAL LOOP


normalLoop :: Bool -> Bool -> Chan.Chan Message -> Package -> Int -> Int -> Int -> IO ()
normalLoop isTerminal warn messageChan rootPkg total successes failures =
  let
    go =
      normalLoop isTerminal warn messageChan rootPkg total
  in
  do  when isTerminal $
          do  hPutStr stdout (renderProgressBar successes failures total)
              hFlush stdout

      update <- Chan.readChan messageChan

      when isTerminal $
          hPutStr stdout clearProgressBar

      case update of
        Close ->
            do  hPutStrLn stdout (closeMessage failures total)
                when (failures > 0) exitFailure

        Complete (CanonicalModule pkg _) localizer path source warnings ->
            do  when (pkg == rootPkg && warn && not (null warnings)) $
                    do  hFlush stdout
                        printSeparator isTerminal Yellow "WARNINGS"
                        mapM_ (printWarning isTerminal localizer path source) warnings

                go (successes + 1) failures

        Error (CanonicalModule pkg _) localizer path source warnings errors ->
            do  hFlush stdout

                when (pkg == rootPkg && warn && not (null warnings)) $
                    do  printSeparator isTerminal Yellow "WARNINGS"
                        mapM_ (printWarning isTerminal localizer path source) warnings


                if pkg == rootPkg
                  then
                    do  when (length warnings + failures > 0) (printSeparator isTerminal Red "ERRORS")
                        mapM_ (printError isTerminal localizer path source) errors

                  else
                    hPutStr stderr (dependencyError pkg)

                go successes (failures + 1)


dependencyError :: Package -> String
dependencyError (pkgName, version) =
    "Problem in dependency " ++ Pkg.toString pkgName ++ " " ++ Pkg.versionToString version ++ "\n"
    ++ "\n"
    ++ "The elm-package.json constraints of '" ++ Pkg.toString pkgName ++ "' are probably\n"
    ++ "letting too much stuff through. Definitely open an issue on the relevant github\n"
    ++ "repo to get this fixed and save other people from this pain.\n"
    ++ "\n"
    ++ "In the meantime, take a look through the direct dependencies of the broken\n"
    ++ "package and see if any of them have had releases recently. If you find the new\n"
    ++ "thing that is causing problems, you can artificially constrain things by adding\n"
    ++ "some extra constraints to your " ++ Path.description ++ " as a stopgap measure.\n\n\n"


printSeparator :: Bool -> Color -> String -> IO ()
printSeparator isTerminal color header =
  let
    total =
      80 - 2 - length header

    left =
      total `div` 2

    right =
      total - left

    mkPad n =
      replicate n '='
  in
    do  when isTerminal $ hSetSGR stderr [SetColor Foreground Dull color]
        hPutStr stderr (mkPad left ++ " " ++ header ++ " " ++ mkPad right ++ "\n\n")
        when isTerminal $ hSetSGR stderr [Reset]


printError :: Bool -> Compiler.Localizer -> FilePath -> String -> Compiler.Error -> IO ()
printError isTerminal localizer path source err =
  if isTerminal then
    Compiler.printError stderr localizer path source err
  else
    hPutStr stderr (Compiler.errorToString localizer path source err)


printWarning :: Bool -> Compiler.Localizer -> FilePath -> String -> Compiler.Warning -> IO ()
printWarning isTerminal localizer path source err =
  if isTerminal then
    Compiler.printWarning stderr localizer path source err
  else
    hPutStr stderr (Compiler.warningToString localizer path source err)


checkIsTerminal :: IO Bool
checkIsTerminal =
  do  outIsTerminal <- hIsTerminalDevice stdout
      errIsTerminal <- hIsTerminalDevice stderr
      return (outIsTerminal && errIsTerminal)



-- PROGRESS BAR


barLength :: Float
barLength =
  50.0


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
  case (failures, total) of
    (0, 1) -> "Success! Compiled 1 module."
    (0, _) -> "Success! Compiled " ++ show total ++ " modules."
    (1, _) -> "Detected errors in 1 module."
    (n, _) -> "Detected errors in " ++ show n ++ " modules."
