import Data.List (sort)

coins = [200, 100, 50, 20, 10, 5, 2, 1]


change _      0    = 1
change []     _    = 0
change (c:cs) val = sum [change cs (val - c * n) | n <- [0..val `div` c]]
