import Test.Hspec

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

fibsIterate = map fst . iterate (\(a,b) -> (b,a+b)) $ (1,2)

problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, even x]

main = hspec $ 
  describe "problem2" $ do
    it "First 10 fibonacci terms" $
     take 10 fibs `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  
    it "Sum of even fibonacci numbers that are less than 4 million" $
      problem2 `shouldBe` 4613732
