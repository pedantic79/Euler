module P004.Problem4Spec (main, spec) where

import Test.Hspec
import P004.Problem4

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem4" $ do
    it "Largest palindrome made from the product of two 2-digit numbers" $
      maxPalin (products 2) `shouldBe` 9009

    it "Largest palindrome made from the product of two 3-digit numbers" $
      problem4 `shouldBe` 906609
