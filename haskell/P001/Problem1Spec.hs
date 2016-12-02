module P001.Problem1Spec (main, spec) where

import Test.Hspec
import P001.Problem1

main  = hspec spec

spec :: Spec
spec =
  describe "problem1" $ do
    it "Sum of all multiples of 3 or 5 below 10" $
      mult3or5 10 `shouldBe` 23

    it "Sum of all the multiples of 3 or 5 below 1000" $
      problem1 `shouldBe` 233168
