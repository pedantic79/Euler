import Data.List ((\\))

num = "1406357289"
primes = [2,3,5,7,11,13,17] :: [Int]
factors = [17,7,2,0]

str2Int :: String -> Integer
str2Int = read

isSpecial = all ((==0) . uncurry mod) . flip zip primes . subNumbers
  where
    subNumbers n = map (read . ($n) . (take 3 .) . drop) [1..7]

-- 3-length permutations
perm3 ns = [a:b:[c] | a <- ns, b <- ns, c <- ns, a/=b && b/=c && c/= a]

-- generate all 3 digit numbers from the third parameter
-- only include those who are divisible by p
-- Then append app to to that.
-- if p is 0, then we just append. This is the case where
-- the last digit needs to be found
fMod p app -- param3
  | p == 0    = flip (:) [] . (++app) -- [param3 ++ app]
  | otherwise = map (++app) . filter isMod0 . perm3
  where
    isMod0 = (==0) . flip mod p . str2Int

-- generate all the pandigit strings where digits_234 are divisible
-- by 2, digits_567 divisible by 7, and digits_890 by 17. digit_1 is
-- the missing digit.
--
-- This generates relatively few candidates (16182 vs 3628800)
gen = foldl (flip cMap) [""]
  where
    cMap p = concatMap (\x -> fMod p x (num \\ x))

problem43 = sum . map str2Int . filter isSpecial . gen $ factors
