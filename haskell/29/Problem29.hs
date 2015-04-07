import Data.List (nub,union)

generate n a = take (n - 1) . iterate (*a) $ (a*a)

distinctPowers a b = concatMap (generate a) [2..b]

problem29 = length . nub $ distinctPowers 100 100


