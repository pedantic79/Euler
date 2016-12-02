module P003.Problem3Spec (main, spec) where
import Test.Hspec
import P003.Problem3

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "problem3" $ do
    it "The prime factors of 13195" $
      prime 13195 `shouldBe` [5, 7, 13, 29]

    it "The largest prime factor of the number 600851475143" $
      problem3 `shouldBe` 6857
