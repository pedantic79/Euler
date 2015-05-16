import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Numbers.Primes (primes,isPrime)
import qualified Data.Vector as V

primeAddend n = go primes []
  where
    go pps@(p:ps) acc
      | p > n  = Nothing
      | s == n = Just (length acc)
      | s < n  = go ps (p:acc)
      | s > n  = go pps i
      | otherwise = Nothing
      where s = sum acc
            i = dropUntil (reverse acc)
    dropUntil xs
      | sum xs <= n = reverse xs
      | otherwise   = dropUntil (tail xs)

reversePrimes = filter isPrime [999999,999997..990000]

p x = (\y -> (y, x)) <$> primeAddend x

-- 147s (10s compiled)
problem50 = snd . maximum . mapMaybe p $ reversePrimes

-----------

primeVec = V.fromList . takeWhile (<10^6) $ primes
primeCheckV = V.generate (10^6) isPrime

fn l = do
  pos <- [0..V.length primeVec - l]
  let sl = V.slice pos l primeVec
  let s = V.sum sl
  guard $ s < (10^6)
  guard $ primeCheckV V.! s
  return s

-- 3.79s (0.5s compiled)
problem50a = head . concatMap fn $ [550,549..1]
