import Test.Hspec

mult3or5 n = sum [ x | x <- [1..(n-1)]
                     , x `mod` 3 == 0 || x `mod` 5 == 0 ]

problem1 = mult3or5 1000

main  = hspec $
  describe "problem1" $ do
    it "Sum of all multiples of 3 or 5 below 10" $
      mult3or5 10 `shouldBe` 23

    it "Sum of all the multiples of 3 or 5 below 1000" $
      problem1 `shouldBe` 233168
