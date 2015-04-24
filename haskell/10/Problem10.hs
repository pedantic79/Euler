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

{-
Prelude> let x = 3
Prelude> let q = 9
Prelude> let fs=[]
Prelude>       filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
[3,5,7]
Prelude> let x = 11
Prelude> let q = 25
Prelude> let fs=[3]
Prelude> [x,x+2..q-2]
[11,13,15,17,19,21,23]
Prelude>       filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
[11,13,17,19,23]
Prelude> let x = 27
Prelude> let q = 49
Prelude> [x,x+2..q-2]
[27,29,31,33,35,37,39,41,43,45,47]
Prelude> let fs=[3,5]
Prelude>       filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
[29,31,37,41,43,47]
Prelude> let x = 51
Prelude> let q = 121
Prelude> let fs=[3,5,7]
Prelude>       filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
[53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

-}
