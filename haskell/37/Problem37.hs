import Data.List (inits)
import qualified Data.Set as Set

str2Int s = read s :: Integer

right :: Integer -> [Integer]
right = map str2Int . tail . inits . show

left :: Integer -> [Integer]
left = map (str2Int . reverse) . tail . inits . reverse . show

primesST = 2 : oddprimes
  where
    oddprimes = sieve 3 9 oddprimes (inits oddprimes)  -- [],[3],[3,5],...
    sieve x q ~(_:t) (fs:ft) =
      filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
      ++ sieve (q+2) (head t^2) t ft

primeSet = Set.fromList $ takeWhile (<1000000) primesST

isPrime n = Set.member n primeSet

trunc = filter (all isPrime . right) .
        filter (all isPrime . left) $ [11,13..1000000]

problem37 = sum trunc
