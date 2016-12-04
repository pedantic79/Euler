module P007.Problem7 (problem7, primes) where
import Data.Maybe (mapMaybe)

primes = 2:mapMaybe factor [3,5..]

factor n = factor' $ takeWhile (\a -> a * a <= n) primes
  where factor' []                      = Just n
        factor' (x:xs) | n `mod` x == 0 = Nothing
                       | otherwise      = factor' xs

problem7 = primes !! 10000
