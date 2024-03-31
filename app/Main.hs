module Main where

import Language.Refal
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getContents >>= either (hPutStrLn stderr) print . parseProgram' "stdin"
