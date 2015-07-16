import Data.Ratio

step = (1/) . (+2)
values = map (+1) . iterate step $ 1%2

numLen = length . show
test r = let n = numLen $ numerator r
             d = numLen $ denominator r
         in n > d

problem57 = length . filter test . take 1000 $ values
            
