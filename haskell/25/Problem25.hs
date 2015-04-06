import Data.Array

fibs = 1:1:zipWith (+) fibs (tail fibs)

fibsLens = scanl (\(n,_) fib -> (n+1, length (show fib))) (0,0) fibs

problem25 = fst . head . filter (\x -> snd x==1000) $ fibsLens
