{- Divide the number by the factor over and over again until the remainder is
   not zero. Store that factor as "largest". If the remainder is not zero,
   then increment the factor by 1 (this is only necessary because 2 -> 3)

   If factor is the square root of m then stop and return the largest

-}

prime m = prime' 2 m m
  where prime' fact num largest
          | fact * fact <= m = if num `mod` fact == 0
                               then prime' fact (num `div` fact) fact
                               else prime' (fact + 1) num largest
          | otherwise        = largest


problem3 = prime 600851475143
