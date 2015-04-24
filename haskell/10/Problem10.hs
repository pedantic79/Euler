import Data.Maybe (mapMaybe)
import Data.List (inits)

{- infinite primes from problem 7 but using Maybe.
   Removed the use of takeWhile, instead we just
   check to see if we are past the checking point.

   We we pass (tail primes), because none of the
   numbers we are checking are even, so they will
   never be divisible by 2.
-}
--primes = 2:3:5:mapMaybe (next (tail primes)) [6*k+r | k<-[1..],r<-[1,5]]
primes = 2:3:mapMaybe (next (tail primes)) [5,7..]
  where next (x:xs) n
          | x * x > n      = Just n
          | n `rem` x == 0 = Nothing
          | otherwise      = next xs n

primesST = 2 : oddprimes
  where
    oddprimes = sieve 3 9 oddprimes (inits oddprimes)  -- [],[3],[3,5],...
    sieve x q ~(_:t) (fs:ft) =
      filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
      ++ sieve (q+2) (head t^2) t ft

-- 142913828922
problem10 = sum $ takeWhile (<2000000) primes
