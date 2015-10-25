import Test.Hspec

{- Divide the number by the factor over and over again until the remainder is
   not zero. Prepend that factor. If the remainder is not zero,
   then increment the factor by 1 (this is only necessary because 2 -> 3)

   If factor is the square root of m then stop 

-}

prime m = prime' 2 m
  where
    prime' fact num
      | fact * fact <= m = if num `mod` fact == 0
                           then fact:prime' fact (num `div` fact)
                           else prime' (fact + 1) num
      | otherwise        = []


problem3 = maximum $ prime 600851475143

main = hspec $
  describe "problem3" $ do
    it "The prime factors of 13195" $
      prime 13195 `shouldBe` [5, 7, 13, 29]
  
    it "The largest prime factor of the number 600851475143" $
      problem3 `shouldBe` 6857
    
