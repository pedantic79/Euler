numToList = aux []
  where
    aux acc 0 = acc
    aux acc n = let (q,r) = n `quotRem` 10
                in aux (r:acc) q

sumDigits = sum . numToList

problem56 = maximum [sumDigits $ a^b | a <- [1..99], b <- [1..99]]


