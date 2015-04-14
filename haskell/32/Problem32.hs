import Data.List (elem, nub, union)

-- Based off the version from problem 12
-- Returns a tuple containing a*b=c
factorize n = factorize' 2
  where factorize' i
          | iSq > n   = [] 
          | iSq == n  = [(i,num,n)]
          | rem == 0  = (i,num,n):next
          | otherwise = next
          where (num, rem) = n `divMod` i
                next = factorize' (i + 1)
                iSq = i * i


isZeroToNine c = c `elem` "123456789"

check (a,b,c) = testComplete (show a) (show b) (show c)
testComplete a b c = iLen == 9 && uLen == 9 && uTst
  where u = nub (a++b++c)
        uLen = length u
        uTst = all isZeroToNine u
        iLen = length (a++b++c)

thd (_,_,c) = c

problem32 = sum . nub . map thd . concatMap (filter check . factorize)
            $ [1..8000]
        
