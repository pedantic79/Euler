import Data.Maybe (mapMaybe)

{- infinite primes from problem 7 but using Maybe.
   Removed the use of takeWhile, instead we just
   check to see if we are past the checking point.

   We we pass (tail primes), because none of the
   numbers we are checking are even, so they will
   never be divisible by 2.
-}
primes = 2:3:mapMaybe (next (tail primes)) [5,7..]
  where next (x:xs) n
          | x * x > n      = Just n
          | n `rem` x == 0 = Nothing
          | otherwise      = next xs n


-- 142913828922
problem10 = sum $ takeWhile (<2000000) primes


