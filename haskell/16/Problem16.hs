sumDigits n | n > 0     = rem + sumDigits quot
            | otherwise = 0
  where (quot, rem) = n `divMod` 10

problem16 = sumDigits $ 2^1000
