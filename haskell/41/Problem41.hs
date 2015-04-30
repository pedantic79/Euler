import Data.List (permutations,sort)
import Data.Numbers.Primes (isPrime)

genPanDigits :: Integer -> [Integer]
genPanDigits n = map read . permutations . concatMap show $ [1..n]

problem41 = findFirst [9,8..1]
  where findFirst (n:ns)
          | null ps = findFirst ns
          | otherwise = maximum ps
          where ps = filter isPrime $ genPanDigits n
