import Data.Numbers.Primes

str2Int :: String -> Int
str2Int = read

primeL p = dropWhile (<10^p) . takeWhile (<2*10^p) $ primes

strReplace fm to = map (\c -> if c == fm then to else c) 

variations fm str = map (str2Int . flip (strReplace fm) str) "0123456789"

primeVars p ch = filter isPrime . filter (>10^p) . variations ch

run p ch = filter ((==8) . length) . map (primeVars p ch . show) $ primeL p

problem51 = head . concat $ run 5 '1'

main = print . head . concat . concat $
       [run n ch | n <- [5], ch <- "1023456789" ]
