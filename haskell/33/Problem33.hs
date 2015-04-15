frac = [ (a,b) | b <- [12..99], a <- [11..b-1]]
isCancellable (x,y) = x `mod` 10 == y `div` 10
cancellable = filter isCancellable frac


cancelReduce (x,y) = realReduce (x `div` 10, y `mod` 10)
realReduce (n,d) = let g = gcd n d in (n `div` g, d `div` g)
mapReduced = map (\a -> (realReduce a, cancelReduce a, a)) cancellable

isReduceSame ((a,b), (c,d),_) = a == c && b == d
thd (_,_,c) = c

nonTrivialFractions = map thd . filter isReduceSame $ mapReduced

p = foldr1 (\(a,b) (c,d) -> (a*c, b*d)) nonTrivialFractions

problem33 = realReduce p
