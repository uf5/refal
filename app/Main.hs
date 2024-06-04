module Main where

import Language.Refal
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let args' = map fromString args
  prog <- getContents
  case parseProgram' "stdin" prog of
    Left err -> hPutStrLn stderr err
    Right parsed -> do
      let desugared = desugarProgram parsed
      result <- evaluate desugared args'
      case result of
        Left err ->
          let (m, t) = showError err
           in hPutStrLn stderr ("Error: " <> m <> "\n" <> unlines (map ("  at " <>) t))
        Right _ -> pure ()
  where
    fromString = OSt . map (OSym . Char)

    showError :: EvaluationError -> (String, [String])
    showError (FnNotDefined f) = ("Function is not defined" <> show f, [])
    showError (VarNotDefined v) = ("Var is not defined" <> pretty v, [])
    showError NoMatchingPattern = ("No matching pattern", [])
    showError DivisionByZero = ("Division by zero", [])
    showError BadArgument = ("Bad argument", [])
    showError NotAValidFunctionName = ("Not a valid function name", [])
    showError (Info msg err) =
      let (m, t) = showError err
       in (m, msg : t)
