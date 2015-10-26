import Test.Hspec

remainder n = all (\x -> n `mod` x == 0) [2..20]

-- 380 = 19 * 20
problem5a = helper 380
  where helper n | remainder n = n
                 | otherwise   = helper (n + 380)

multiples n = foldl1 lcm [1..n]

problem5 = multiples 20

main = hspec $
  describe "problem5" $ do
    it "Smallest number divisible by all numbers 1 to 10" $
      multiples 10 `shouldBe` 2520

    it "Smallest number divisible by all numbers 1 to 20" $
      problem5 `shouldBe` 232792560
