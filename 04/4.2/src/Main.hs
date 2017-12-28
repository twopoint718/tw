module Main where


import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hPrint, stderr)

import           Compiler           (compileString)


main :: IO ()
main = do
  files <- getArgs
  let sourceFile = case files of
        (filename:_) -> filename
        _            -> error "Usage: gossamerc <FILE>"
  sourceCode <- readFile sourceFile
  case compileString sourceFile sourceCode of
    Left err -> do
      hPrint stderr ("ERRORS: " ++ err)
      exitFailure
    Right wasm -> do
      print wasm
      exitSuccess
