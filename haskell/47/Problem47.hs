import Data.Numbers.Primes (primeFactors)
import Data.List (nub,tails)

len4Factors = filter ((==4) . length . nub . primeFactors) [2..]

find4Cons (n:ns) acc
  | length acc == 4     = reverse acc
  | null acc            = find4Cons ns [n]
  | (n - 1) == head acc = find4Cons ns (n:acc)
  | otherwise           = find4Cons ns [n]

find4Cons [] acc
  | length acc == 4 = reverse acc
  | otherwise       = []

find4Cons2 :: (Ord t, Enum t) => [t] -> [t]
find4Cons2 = head . filter (isConsec) .  map (take 4) . tails

isConsec xs = xs == [(minimum xs) .. (maximum xs)]

problem47 = head $ find4Cons len4Factors []

problem47b = head . find4Cons2 $ len4Factors
