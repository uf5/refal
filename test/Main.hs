module Main where

import Language.Refal
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pattern matching" $ do
    it "s-type" $ do
      matchPattern
        [PVar (Var S "foo"), PVar (Var S "bar"), PVar (Var S "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 1)]
        `shouldBe` Just
          [ (Var S "bar", OSym (Int 2)),
            (Var S "foo", OSym (Int 1))
          ]
    it "e-type" $ do
      matchPattern
        [PVar (Var E "foo"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          [ (Var E "foo", OSt [OSym (Int 1), OSym (Int 2), OSym (Int 3)])
          ]
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "bar")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          [ (Var E "bar", OSt [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
            (Var E "foo", OSt [])
          ]
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "bar"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          [ (Var E "bar", OSt [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
            (Var E "foo", OSt [])
          ]
    it "structural parentheses test" $ do
      matchPattern
        [PSt [PVar (Var E "foo"), PVar (Var E "bar"), PSym (Int 0)]]
        [OSt [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]]
        `shouldBe` Just
          [ (Var E "bar", OSt [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
            (Var E "foo", OSt [])
          ]
