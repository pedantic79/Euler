import Test.Hspec

products l = [a * b | a <- s, b <- s]
  where s = [10^(l-1) .. 10^l-1]

maxPalin = maximum . filter (\x -> isPalin (show x))
  where isPalin xs = xs == reverse xs

problem4 = maxPalin $ products 3

main = hspec $ 
  describe "problem4" $ do
    it "Largest palindrome made from the product of two 2-digit numbers" $
      maxPalin (products 2) `shouldBe` 9009
  
    it "Largest palindrome made from the product of two 3-digit numbers" $
      problem4 `shouldBe` 906609
