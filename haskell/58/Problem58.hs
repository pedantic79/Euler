import Data.List (inits,findIndex)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)

-- Taken from problem 28
rowStp = map (\x -> ((x - 2)^2+1, x^2, x-2)) [3,5..]

integerSqrt = floor . sqrt . fromIntegral

-- Produce the corner values
corners :: [[Int]]
corners = map mkRow rowStp
  where
    mkRow (l,h,s) = let a = l + s
                        b = a + s + 1
                    in [a, b .. h]
      
primeCorners = map (length . filter isPrime) corners
count = let num = 5 : repeat 4
        in zip primeCorners num

intPerc x y = x * 100 `div` y

percentage = uncurry intPerc . sums
  where sums = foldr1 (\(a,b) (c,d) -> (a+c, b+d))

f = fromJust . findIndex (<10) . map percentage . tail . inits $ count

-- 26241
-- 47.09s/14.880s
problem58a = integerSqrt . last $ corners !! f

percentMap = pMap 0 0
  where
    pMap pCnt tot ((p,q):ps) =
      let pCnt' = p + pCnt
          tot' = q + tot
      in intPerc pCnt' tot' : pMap pCnt' tot' ps

--  9.98s/0.439s
problem58 = integerSqrt . last $ corners !! l
  where l = fromJust . findIndex (<10) . percentMap $ count

main = print problem58
        
