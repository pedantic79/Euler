fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
fibsIterate = (map fst . iterate (\(a,b) -> (b,a+b)) (1,2)

problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, even x]



