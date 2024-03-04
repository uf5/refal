module Main where

import Control.Monad.Identity
import Control.Monad.State
import Language.Refal
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "pattern matching" $ do
    it "s-type" $ do
      testPattern
        [PVar (Var S "foo"), PVar (Var S "bar"), PVar (Var S "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 1)]
        `shouldBe` Right
          [ (Var S "bar", OSym (Int 2)),
            (Var S "foo", OSym (Int 1))
          ]
  where
    testPattern p o = runIdentity (runParserT (execStateT (mkPOP p) []) "" o)

-- main :: IO ()
-- main = do
--   print $ testPattern p o
--   print $ testPattern p' o'
--   print $ testPattern p'' o''
--   where
--     p = [PVar (Var S "foo"), PVar (Var S "foo"), PVar (Var S "bar")]
--     o = [OSym (Int 1), OSym (Int 1), OSym (Int 3)]
--     p' = [PVar (Var T "foo"), PVar (Var T "foo")]
--     o' = [OSt [OSym (Int 1), OSym (Int 2)], OSt [OSym (Int 1), OSym (Int 2)]]
--     p'' = [PVar (Var T "foo"), PVar (Var E "bar")]
--     o'' = [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSt [OSym (Int 4), OSym (Int 5)]]
--
-- testPattern p o = either2maybe (runIdentity (runParserT (runStateT (mkPOP p) []) "" o))
--   where
--     either2maybe (Right a) = pure a
--     either2maybe _ = Nothing
