import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)


primes = 2:3:mapMaybe (next (tail primes)) [5,7..]
  where next (x:xs) n
          | x * x > n      = Just n
          | n `rem` x == 0 = Nothing
          | otherwise      = next xs n

{- Using elem won't work because primes is infinite
   But since primes is in order, we can drop all numbers
   less than n, and compare if the first number remaining
   is the same.
-}
isPrime n = n == (head . dropWhile (<n) $ primes)

testConsecutivePrime f = tcp' 0
  where tcp' n | isPrime (f n) = tcp' (n + 1)
               | otherwise     = n

q a b n = n * n + a*n + b

-- b must be prime, because for q a b 0 = b
combinations = [ (testConsecutivePrime (q a b), a*b)
               | a <- [-1000..1000]
               , b <- takeWhile (<1000) primes
               ]

filterFst t = filter (t . fst)

problem27 = snd . maximumBy (comparing fst) . filterFst (>40) $ combinations
              
