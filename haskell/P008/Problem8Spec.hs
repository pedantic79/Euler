module P008.Problem8Spec (main, spec) where

import Test.Hspec
import P008.Problem8

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem8" $ do
    it "The 4 adjacent digits that have the greatest product." $
      prod 4 numbers `shouldBe` 5832

    it "The 13 adjacent digits that have the greatest product." $
      problem8 `shouldBe` 23514624000
