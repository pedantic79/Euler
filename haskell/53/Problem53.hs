import qualified Data.Vector as V

factV = V.generate 101 (\n -> product [2..(toInteger n)])
fact n = factV V.! n

nCr n r = fact n `div` (fact r * fact (n-r))

problem53 = length [ (n,r) | n <- [1..100], r <- [1..n], nCr n r > 1000000 ]
             
