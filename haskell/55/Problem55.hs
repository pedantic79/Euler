isPalindrone xs = go [] xs xs
  where
    go rev (x:xs) (_:_:ys) = go (x:rev) xs ys
    go rev (x:xs) [_] = rev == xs
    go rev xs [] = rev == xs

isNumPalin :: Integer -> Bool
isNumPalin = isPalindrone . show

revNum = aux 0
  where
    aux acc 0 = acc
    aux acc x = let (x', r) = x `quotRem` 10
                in aux (10 * acc + r) x'

step n = n + revNum n                   

checkLychrel = aux 0
  where
    aux 30 m = True
    aux c  m = let m' = m + revNum m
               in if isNumPalin m'
                  then False
                  else aux (c + 1) m'

problem55 = length . filter checkLychrel $ [1..10000]
