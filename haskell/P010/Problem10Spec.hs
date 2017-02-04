module P010.Problem10Spec (main, spec) where

import Test.Hspec
import P010.Problem10

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem10" $ do
    it "Sum of primes below 10" $
      example10 `shouldBe` 17
    it "Sum of primes below 200000" $
      problem10 `shouldBe` 142913828922
