module P005.Problem5 (multiples, problem5) where

remainder n = all (\x -> n `mod` x == 0) [2..20]

-- 380 = 19 * 20
problem5a = helper 380
  where helper n | remainder n = n
                 | otherwise   = helper (n + 380)

multiples n = foldl1 lcm [1..n]

problem5 = multiples 20
