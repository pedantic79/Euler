module P009.Problem9Spec (main, spec) where

import Test.Hspec
import P009.Problem9

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem9" $
    it "Product of the Pythagorean triplet for which a + b + c = 1000." $
      problem9 `shouldBe` 31875000
