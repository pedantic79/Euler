import Data.List (maximumBy,foldl1')
import Data.Ord (comparing)

s :: Integer -> [Integer]
s n | n == 1         = [1]
    | n `mod` 2 == 0 = n : s (n `div` 2)
    | otherwise      = n : s (3 * n + 1)

seqLen = length . s

lens = map (\x -> (x, seqLen x)) [2..1000000]

-- This will stack overflow when compiled without optimization
-- alternatively, we can use foldl1' to avoid thunking
-- real    0m4.880s
-- user    0m1.160s
-- sys     0m3.636s
problem14 = foldl1' longest lens
  where longest a@(_,a2) b@(_,b2) | a2 >= b2  = a
                                  | otherwise = b

-- This will stack overflow when compiled without optimization
-- real    0m20.952s
-- user    0m5.976s
-- sys     0m14.872s
problem14a = maximumBy (comparing seqLen) [2..1000000]

main = print problem14
