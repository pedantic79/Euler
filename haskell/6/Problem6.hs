import Test.Hspec

sumOfSquares = sum . map (^2)
squareOfSums = (^2) . sum

diff n = squareOfSums [1..n] - sumOfSquares [1..n]

problem6 = diff 100

main = hspec $
  describe "problem6" $ do
    it "Square of the sums of 1 to 10 - sum of squares of 1 to 10" $
      diff 10 `shouldBe` 2640

    it "Square of the sums of 1 to 100 - sum of squares of 1 to 100" $
      problem6 `shouldBe` 25164150
