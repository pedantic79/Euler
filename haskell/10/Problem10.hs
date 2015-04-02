-- infinite primes from problem 7
primes = 2:((filter (/=0) . map next) [3,5..])

next n = next' $ takeWhile (\x -> x * x <= n) primes
  where
    next' [] = n
    next' (x:xs) | n `mod` x == 0 = 0
                 | otherwise      = next' xs


problem10 = sum $ takeWhile (<2000000) primes


