import Data.List (elem, nub, union)

-- Based off the version from problem 12
-- Returns [(a,b,c), ...] where a * b = c
factorize n = factorize' 2
  where factorize' i
          | iSq > n   = [] 
          | iSq == n  = [(i,num,n)]
          | rem == 0  = (i,num,n):next
          | otherwise = next
          where (num, rem) = n `divMod` i
                next = factorize' (i + 1)
                iSq = i * i

-- Check if the character is 1 through 9
isZeroToNine = (`elem` "123456789")

-- run testComplete but with a tuple of numbers
check (a,b,c) = testComplete (show a) (show b) (show c)

-- Make sure that the length of a+b+c is 9
-- Make sure that once we nub a+b+c it is also 9
-- Make sure nub a+b+c is only 1 through 9
testComplete a b c = iLen == 9 && uLen == 9 && uTst
  where u = nub (a++b++c)
        uLen = length u
        uTst = all isZeroToNine u
        iLen = length (a++b++c)

-- Get the third elemnt
thd (_,_,c) = c

listOfPanNum = concatMap (filter check . factorize) $ [1..10000]
problem32 = sum . nub . map thd $ listOfPanNum
        
