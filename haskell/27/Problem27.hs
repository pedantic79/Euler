import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)


primes = 2:3:mapMaybe (next (tail primes)) [5,7..]
  where next (x:xs) n
          | x * x > n      = Just n
          | n `rem` x == 0 = Nothing
          | otherwise      = next xs n

isPrime n = n == (head . dropWhile (<n) $ primes)

testConsPrime f = tcp' 0
  where tcp' n | isPrime (f n) = tcp' (n + 1)
               | otherwise     = n

q a b n = n * n + a*n + b

combinations = [ (testConsPrime (q a b), a*b)
               | a <- [-1000..1000]
               , b <- [-1000..1000]
               ]

filterFst t = filter (t . fst)

problem27 = snd . maximumBy (comparing fst) . filterFst (>40) $ combinations
              
