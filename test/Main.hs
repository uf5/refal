module Main where

import Language.Refal
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pattern matching" $ do
    it "st" $ do
      matchPattern
        [PVar (SType (SVar "foo")), PSt [PVar (SType (SVar "foo")), PVar (SType (SVar "bar"))], PVar (SType (SVar "foo"))]
        [OSym (Int 1), OSt [OSym (Int 1), OSym (Int 2)], OSym (Int 1)]
        `shouldBe` Just
          ( mempty
              { sType =
                  [ (SVar "bar", Int 2),
                    (SVar "foo", Int 1)
                  ]
              }
          )
    it "s-type" $ do
      matchPattern
        [PVar (SType (SVar "foo")), PVar (SType (SVar "bar")), PVar (SType (SVar "foo"))]
        [OSym (Int 1), OSym (Int 2), OSym (Int 1)]
        `shouldBe` Just
          ( mempty
              { sType =
                  [ (SVar "bar", Int 2),
                    (SVar "foo", Int 1)
                  ]
              }
          )
    it "e-type" $ do
      matchPattern
        [PVar (SType $ SVar "bar"), PVar (EType $ EVar "foo")]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType =
                  [(SVar "bar", Int 0)],
                eType =
                  [ (EVar "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)])
                  ]
              }
          )
      matchPattern
        [PVar (SType $ SVar "bar"), PVar (EType $ EVar "foo"), PSym (Int 2)]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType = [(SVar "bar", Int 0)],
                eType = [(EVar "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 1)])]
              }
          )
      matchPattern
        [PVar (EType $ EVar "foo"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          ( mempty
              { eType = [(EVar "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])]
              }
          )
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "bar")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (EVar "bar", [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
                    (EVar "foo", [])
                  ]
              }
          )

      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "bar"), PSym (Int 0)]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 0)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (EVar "bar", [OSym (Int 1), OSym (Int 2), OSym (Int 3)]),
                    (EVar "foo", [])
                  ]
              }
          )
    it "e-type repeating" $ do
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (EVar "foo", [OSym (Int 1), OSym (Int 2)])
                  ]
              }
          )
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (EVar "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])
                  ]
              }
          )
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        [OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3), OSym (Int 1), OSym (Int 2), OSym (Int 3)]
        `shouldBe` Just
          ( mempty
              { eType =
                  [ (EVar "foo", [OSym (Int 1), OSym (Int 2), OSym (Int 3)])
                  ]
              }
          )
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        []
        `shouldBe` Just
          (mempty {eType = [(EVar "foo", [])]})
      matchPattern
        [PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        []
        `shouldBe` Just
          (mempty {eType = [(EVar "foo", [])]})
      matchPattern
        [PVar (SType $ SVar "bar"), PVar (EType $ EVar "foo"), PVar (EType $ EVar "foo")]
        [OSym (Int 0), OSym (Int 1), OSym (Int 2), OSym (Int 1), OSym (Int 2)]
        `shouldBe` Just
          ( mempty
              { sType = [(SVar "bar", Int 0)],
                eType = [(EVar "foo", [OSym (Int 1), OSym (Int 2)])]
              }
          )
