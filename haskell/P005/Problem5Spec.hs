module P005.Problem5Spec (main, spec) where

import Test.Hspec
import P005.Problem5

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem5" $ do
    it "Smallest number divisible by all numbers 1 to 10" $
      multiples 10 `shouldBe` 2520

    it "Smallest number divisible by all numbers 1 to 20" $
      problem5 `shouldBe` 232792560
