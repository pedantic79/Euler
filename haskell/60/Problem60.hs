import Data.Numbers.Primes (isPrime,primes)


concatNum a b = read $ show a ++ show b :: Integer

checkConcat xs x = all isPrime q
  where
    q = [concatNum i x | i<-xs] ++ [concatNum x i | i<-xs]

{- The accumulator stores our "valid" answer
   if p is checked as true, we prepend to our accumulator
   and decrease the length. If p is false then do nothing.

   We concat the previous with the path where we do not
   append and only traverse the primes

   Effectively, for the first call (where acc is []). This
   produces a list of list of primes i.e. [[2],[3],[5],[7]...]

   The next call we check each of those lists, with the remaining primes
   e.g. gen [2] 4 [3,5,7,11..]
        gen [3] 4 [5,7,11,13..]
        gen [5] 4 [7,11,13,17..]
-}
gen acc len (p:ps)
  | null ps   = []
  | len == 0  = [acc]
  | otherwise =  (if checkConcat acc p
                 then gen (p:acc) (len - 1) ps
                 else [])
                      ++ gen acc len ps


{- Unlike gen, gen' uses a bottom up approach rather than top down
   The base case is when we ask for len 1. Like above, it's a list
   of list of all the primes. So we have a case for that

   For the default case, we are going to use list comprehension to
   search through the lists. p <- ps to interate through the primes.
   f <- (recursive-call) to iterate through all the solutions with
   1 less length.

   The trick is that we filter the primes. Firstly, only include
   primes greater than our intial p. Second, perform the check in
   the problem. This means the p and the primes we've filter are
   meet the check.
-}
gen' len ps
  | len == 1  = map (:[]) ps
  | otherwise = [ p:f | p <- ps, f <- gen' (len - 1) (filt p ps) ]
  where
    filt x = filter $ \n -> n > x && check x n && check n x
    check a  = isPrime . concatNum a

m = 10000
p = takeWhile (<m) primes

-- 26033 / 52.88s / 2.26s (compiled)
problem60 = sum . head $ gen [] 5 p

-- 26033 / 2.74s / 0.22s (compiled)
problem60' = sum . head $ gen' 5 p

main = print problem60'

