module Main where
import Data.Array ((!), listArray)
import Data.Maybe (mapMaybe)
import Debug.Trace

factorize n = 1:factorize' 2
  where factorize' i | iSq > n   = [] 
                     | iSq == n  = [i]
                     | rem == 0  = i:num:next
                     | otherwise = next
          where (num, rem) = n `divMod` i
                next = factorize' (i + 1)
                iSq = i * i

factorSum = sum . factorize

isAbundant n = factorSum n > n
isAbundantMaybe n = if isAbundant n then Just n else Nothing

{- I was using elem to determine if a number was already in list
   of abundant numbers I had. This is super duper slow. Changing
   it to call isAbundant improved performance, since the elem call
   is a O(n) operation vs a O(1).

   | isAbundant (n-a) = False
   real    0m19.374s
   user    0m6.952s
   sys     0m12.312s

   | elem (n-a) smallAbundant = False
   real    4m2.999s
   user    4m1.512s
   sys     0m0.252s
-}
smallAbundant = mapMaybe isAbundantMaybe [1..28124]
isNotTwoAbundants n = isNotTwoAbundants' smallAbundant
  where isNotTwoAbundants' [] = True
        isNotTwoAbundants' (a:as)
          | a > n `quot` 2 = True
          | isAbundant (n-a) = False
          | otherwise = isNotTwoAbundants' as


{- Here we can improve peformance even more if instead we use an Array.
   So we construct an array with the precomputed isAbundant values.
   Here we can simply check if a number is abundant by checking it
   against the array. We could squeeze out more performance by
   looping over the smallAbundant list, similar to isNotTwoAbundants.

   real    0m0.682s
   user    0m0.204s
   sys     0m0.412s
-}

abundantArray = listArray (1,28124) (map isAbundant [1..28124])
isNotAbundantArray n = isNotAbundantArray' 1
  where isNotAbundantArray' i
          | i > n `quot` 2 = True
          | abundantArray ! i && abundantArray ! (n - i) = False
          | otherwise = isNotAbundantArray' (i+1)


problem23a = sum . filter isNotAbundantArray $ [1..28124]                       
        
problem23 = sum . filter isNotTwoAbundants $ [1..28124]
main = print problem23a




