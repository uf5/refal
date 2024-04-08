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
          ( mempty
              { sType =
                  [ (Var S "bar", Int 2),
                    (Var S "foo", Int 1)
                  ]
              }
          )
    it "e-type" $ do
      matchPattern
        [PVar (Var S "bar"), PVar (Var E "foo")]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType =
                  [(Var S "bar", Int 0)],
                eType =
                  [ (Var E "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)])
                  ]
              }
          )
      matchPattern
        [PVar (Var S "bar"), PVar (Var E "foo"), PSym (Int 2)]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType = [(Var S "bar", Int 0)],
                eType = [(Var E "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 1)])]
              }
          )
      matchPattern
        [PVar (Var E "foo"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          ( mempty
              { eType = [(Var E "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])]
              }
          )
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "bar")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (Var E "bar", [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
                    (Var E "foo", [])
                  ]
              }
          )

      matchPattern
        [PVar (Var E "foo"), PVar (Var E "bar"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (Var E "bar", [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
                    (Var E "foo", [])
                  ]
              }
          )
    it "e-type repeating" $ do
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (Var E "foo", [OSym (Int 1), OSym (Int 2)])
                  ]
              }
          )
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (Var E "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])
                  ]
              }
          )
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "foo"), PVar (Var E "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (Var E "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])
                  ]
              }
          )
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "foo"), PVar (Var E "foo")]
        []
        `shouldBe` Just
          (mempty {eType = [(Var E "foo", [])]})
      matchPattern
        [PVar (Var E "foo"), PVar (Var E "foo")]
        []
        `shouldBe` Just
          (mempty {eType = [(Var E "foo", [])]})
      matchPattern
        [PVar (Var S "bar"), PVar (Var E "foo"), PVar (Var E "foo")]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType = [(Var S "bar", Int 0)],
                eType = [(Var E "foo", [OSym (Int 1), OSym (Int 2)])]
              }
          )
