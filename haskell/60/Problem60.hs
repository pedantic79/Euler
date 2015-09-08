import Data.Numbers.Primes (isPrime,primes)



checkConcat xs x = all isPrime q
  where
    q = [concatNum i x | i<-xs] ++ [concatNum x i | i<-xs]
    concatNum a b = read $ show a ++ show b :: Int

gen acc len (p:ps)
  | null ps   = []
  | len == 0  = [acc]
  | otherwise =  (if checkConcat acc p
                 then gen (p:acc) (len - 1) ps
                 else [])
                      ++ gen acc len ps

m = 10000
p = takeWhile (<m) primes
problem60 = sum . head $ gen [] 5 p

-- 26033 / 52.88s / 2.26s (compiled)
main = print problem60

