module P006.Problem6Spec (main, spec) where

import Test.Hspec
import P006.Problem6

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem6" $ do
    it "Square of the sums of 1 to 10 - sum of squares of 1 to 10" $
      diff 10 `shouldBe` 2640

    it "Square of the sums of 1 to 100 - sum of squares of 1 to 100" $
      problem6 `shouldBe` 25164150
