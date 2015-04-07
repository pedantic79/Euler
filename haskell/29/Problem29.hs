import Data.List (nub,union)

generate b' a = take (b' - 1) . iterate (*a) $ (a*a)

distinctPowers a b = concatMap (generate b) [2..a]

problem29 = length . nub $ distinctPowers 100 100


