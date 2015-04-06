import Data.List (permutations, sort)

problem24 = (sort . permutations) "0123456789" !! 999999


