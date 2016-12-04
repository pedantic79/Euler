module P007.Problem7Spec (main, spec) where

import Test.Hspec
import P007.Problem7

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem7" $ do
    it "The 6th prime number" $
      primes !! 5 `shouldBe` 13

    it "The 10001th prime number" $
      problem7 `shouldBe` 104743
