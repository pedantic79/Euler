module P002.Problem2Spec (main, spec) where

import Test.Hspec
import P002.Problem2

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem2" $ do
    it "First 10 fibonacci terms" $
     take 10 fibs `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

    it "Sum of even fibonacci numbers that are less than 4 million" $
      problem2 `shouldBe` 4613732
