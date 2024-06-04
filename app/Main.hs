module Main where

import Language.Refal
import System.Environment (getArgs)
import System.IO (hPrint, hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let args' = map fromString args
  prog <- getContents
  case parseProgram' "stdin" prog of
    Left err -> hPutStrLn stderr err
    Right parsed -> do
      let desugared = desugarProgram parsed
      putStrLn "DESUGARED:"
      putStrLn $ pretty desugared
      putStrLn "EVALUATION:"
      case evaluate desugared args' of
        Left err -> hPrint stderr err
        Right res -> putStrLn $ asOutput res
  where
    fromString = OSt . map (OSym . Char)
    asOutput [] = ""
    asOutput ((OSym x) : xs) =
      ( case x of
          (Char c) -> [c]
          (Int i) -> show i
      )
        <> asOutput xs
    asOutput ((OSt xs) : ys) = "(" <> asOutput xs <> ")" <> asOutput ys
