module P011.Problem11Spec (main, spec) where

import Test.Hspec
import P011.Problem11

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem11" $
    it "Largest product in a grid" $
      problem11 `shouldBe` 70600674
