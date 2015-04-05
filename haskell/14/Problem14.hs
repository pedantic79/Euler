import Data.List (maximumBy,foldl1')
import Data.Ord (comparing)

collatz :: Integer -> [Integer]
collatz n | n == 1    = [1]
          | even n    = n : collatz (n `div` 2)
          | otherwise = n : collatz (3 * n + 1)

collatzLen = length . collatz

lengths = map (\x -> (x, collatzLen x)) [2..1000000]

{- Using foldl1 will stack overflow when compiled without optimization
   alternatively, we can use foldl1' to avoid thunking
   real    0m4.880s
   user    0m1.160s
   sys     0m3.636s

   In ghci, this isn't a problem.
   *Main> problem14
   837799
   (55.90s, 53065318736 bytes)
-}
problem14 = fst $ foldl1' longest lengths
  where longest a@(_,a2) b@(_,b2) | a2 >= b2  = a
                                  | otherwise = b

-- This will stack overflow when compiled without optimization
-- real    0m20.952s
-- user    0m5.976s
-- sys     0m14.872s
problem14a = maximumBy (comparing collatzLen) [2..1000000]

main = print problem14
