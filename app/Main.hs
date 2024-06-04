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
      -- putStrLn "DESUGARED:"
      -- putStrLn $ pretty desugared
      -- putStrLn "EVALUATION:"
      case evaluate desugared args' of
        Left err ->
          let (m, t) = showError err
           in hPutStrLn stderr ("Error: " <> m <> "\n" <> unlines (map ("  at " <>) t))
        Right res -> putStrLn $ showOutput res
  where
    fromString = OSt . map (OSym . Char)

    showOutput [] = ""
    showOutput ((OSym x) : xs) =
      ( case x of
          (Char c) -> [c]
          (Int i) -> show i
      )
        <> showOutput xs
    showOutput ((OSt xs) : ys) = "(" <> showOutput xs <> ")" <> showOutput ys

    showError :: EvaluationError -> (String, [String])
    showError (FnNotDefined f) = ("Function is not defined" <> show f, [])
    showError (VarNotDefined v) = ("Var is not defined" <> pretty v, [])
    showError NoMatchingPattern = ("No matching pattern", [])
    showError DivisionByZero = ("Division by zero", [])
    showError BadArgument = ("Bad argument", [])
    showError (Info msg err) =
      let (m, t) = showError err
       in (m, msg : t)

-- showError (FnNotDefined f) = "Function is not defined: " <> f
-- showError (VarNotDefined v) = "Var is not defined: " <> pretty v
-- showError NoMatchingPattern = "No matching pattern"
-- showError DivisionByZero = "Division by zero"
-- showError BadArgument = "Bad argument"
-- showError (Info msg err) = showError err <> "\ninside call of " <> msg
