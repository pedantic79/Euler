import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

-- From problem 10
primes = 2:3:mapMaybe (next (tail primes)) [5,7..]
  where next (x:xs) n | x * x > n      = Just n
                      | n `rem` x == 0 = Nothing
                      | otherwise      = next xs n

{- For a number 'n', calculate 10^m (mod n) = 1
   m will never be greater than n. Return this
   as a Maybe (input, output).

   Using a tuple allows us find the input with the
   largest output.
-}
multOrder n = helper 10 1
  where helper x m | m > n          = Nothing
                   | x `rem` n == 1 = Just (n, m)
                   | otherwise      = helper (10 * x) (m + 1)

-- Only take primes between 900 and 1000
primesList = takeWhile (<1000) . dropWhile (<900) $ primes
primeMultOrder = mapMaybe multOrder primesList
problem26 =  fst $ maximumBy (comparing snd) primeMultOrder
