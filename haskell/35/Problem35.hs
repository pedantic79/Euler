import Data.List (inits)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

-- primeST from haskell wiki
primes = 2 : oddprimes
  where
    oddprimes = sieve 3 9 oddprimes (inits oddprimes)  -- [],[3],[3,5],...
    sieve x q ~(_:t) (fs:ft) =
      filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
      ++ sieve (q+2) (head t^2) t ft
      
rotateNum n = helper n
  where
    (num,pow10) = numLen n (1,1)    
    helper m = take num $ nextNum:helper nextNum
      where nextNum = q + r
            (q,r) = (m * 10) `divMod` pow10
    numLen a (n,acc)
      | a < 10    = (n, 10*acc)
      | otherwise = numLen (a `div` 10) (n+1, 10*acc)

check s n
  | all (isPrime s) $ rotateNum n = Just n
  | otherwise                     = Nothing
  where isPrime = flip Set.member

problem35 = succ . length $ mapMaybe (check primeSet) [3,5..limit]
  where
    primeSet = Set.fromList $ takeWhile (<limit) primes    
    limit = 1000000
