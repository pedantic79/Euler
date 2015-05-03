import Data.List (permutations,(\\))

num = "1406357289"
primes = [2,3,5,7,11,13,17] :: [Int]
factors = [17,7,2,1]

str2Int :: String -> Integer
str2Int = read

isSpecial = all ((==0) . uncurry mod) . flip zip primes . subNumbers
  where
    subNumbers n = map (read . ($n) . (take 3 .) . drop) [1..7]

genSubStr ns = [a:b:[c] | a <- ns, b <- ns, c <-ns, a/=b && b/=c && c/= a]

-- generate all 3 digit numbers from the third parameter
-- only include those who are divisible by p
-- Then append app to to that.
-- if p is 1, then we just append. This is the case where
-- the last digit needs to be found
fDivisible p app 
  | p == 1    = flip (:) [] . (++app) -- [param3 ++ app]
  | otherwise = map (++app) . filter isMod0 . genSubStr
  where
    isMod0 = (==0) . flip mod p . str2Int

-- generate all the pandigit strings where digits_234 are divisible
-- by 2, digits_567 divisible by 7, and digits_890 by 17. digit_1 is
-- the missing digit.
--
-- We go right to left, with factors passed to (p:ps) and [""] as our
-- initial value. This will generate digits_890 that are divisible by
-- 17. Then we loop with the list coming from p=17 for p=7.
-- For p=7, the we map over each acc value, and use fDivisible to
-- generate all digits divisible by 7 that are coming from the set
-- that do not include the acc value being mapped.
-- 
-- This generates relatively few candidates.
gen []     acc = acc
gen (p:ps) acc = gen ps nAcc
  where nAcc = concatMap (\x -> fDivisible p x (num \\ x)) acc

problem43 = sum . map str2Int . filter isSpecial . gen factors $ [""]

