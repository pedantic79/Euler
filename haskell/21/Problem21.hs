import Data.Maybe (mapMaybe)

factorize n = 1:factorize' 2
  where factorize' i | iSq > n   = [] 
                     | iSq == n  = [i]
                     | rem == 0  = i:num:next
                     | otherwise = next
          where (num, rem) = n `divMod` i
                next = factorize' (i + 1)
                iSq = i * i

factorSum = sum . factorize

isAmicable n | a == b    = Nothing
             | n == b    = Just n
             | otherwise = Nothing
  where a = factorSum n
        b = factorSum a

problem21 = sum $ mapMaybe isAmicable [1..10000]
