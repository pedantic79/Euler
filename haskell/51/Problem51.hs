import Data.Numbers.Primes
import Data.List (elemIndices)

str2Int :: String -> Int
str2Int = read

primeL p = dropWhile (<10^p) . takeWhile (<2*10^p) $ primes
strReplace fm to = map (\c -> if c == fm then to else c) 
variations fm str = map (str2Int . flip (strReplace fm) str) "0123456789"
primeVars p ch = filter isPrime . filter (>10^p) . variations ch
run p ch = filter ((==8) . length) . map (primeVars p ch . show) $ primeL p
problem51 = head . concat $ run 5 '1'


f = head [ str2Int p | p <- map show primes, any (check p) "0123456789" ]
repStr fr to str = read $ map (\c -> if c == fr then to else c) str

check p d = length idxs >= 3 && length vartns == 8
  where idxs = elemIndices d p
        vartns = [ r | r <- "0123456789"
                     , isPrime (repStr d r p)
                     , r /= '0' || (0 `notElem` idxs)]
