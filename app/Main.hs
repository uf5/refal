module Main where

import Language.Refal
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  opts <- getArgs
  prog <- getContents
  let prog' = parseProgram' "stdin" prog
  either (hPutStrLn stderr) (print . (`evaluate` Opts opts)) prog'
