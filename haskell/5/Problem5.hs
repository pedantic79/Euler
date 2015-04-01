remainder n = and $ map (\x -> n `mod` x == 0) [2..20]

problem5a = helper 380
  where helper n | remainder n = n
                 | otherwise   = helper (n + 380)

problem5 = foldl1 lcm [1..20]                                 
