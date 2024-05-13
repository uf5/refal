module Main where

import Language.Refal
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  opts <- getArgs
  prog <- getContents
  either
    (hPutStrLn stderr)
    (print . (`evaluate` Opts opts))
    ( do
        parsed <- parseProgram' "stdin" prog
        pure $ desugarProgram parsed
    )
