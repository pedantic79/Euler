hex n = n * (2*n - 1)
pen n = n * (3*n - 1) `div` 2
tri n = n * (n + 1) `div` 2

quadratic a b c = (sq - b)/(2*a)
  where sq = sqrt(b**2 - 4*a*c)

isPen v = fromInteger (pen rev) == v
  where rev = floor $ quadratic 3 (-1) (-2*v)

isTri v = fromInteger (tri rev) == v
  where rev = floor $ quadratic 1 1 (-2*v)

isPenTri v = isPen v && isTri v

problem45 = floor . head . filter isPenTri . map hex $ [144..]






