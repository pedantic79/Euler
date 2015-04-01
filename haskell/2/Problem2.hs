fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, even x]



