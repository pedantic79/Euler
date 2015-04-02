triangles = map (\n -> sum [1..n]) [1..]

-- returns a pair containing the number being
-- factorize and the number of factors
factorize n = (n,  length (factorize' 1))
  where factorize' i | iSq > n   = []
                     | iSq == n  = [i]
                     | rem == 0  = i:num:next
                     | otherwise = next
          where (num, rem) = n `divMod` i
                next = factorize' (i + 1)
                iSq = i * i

triFactors = map factorize triangles
problem12 = head $ filter (\(a,b) -> b > 500) triFactors
