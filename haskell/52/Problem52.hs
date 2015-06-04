import Data.List (sort)

sortNum = sort . show

allTheSame (x:xs) = let y = sortNum x
                    in all (\n -> sortNum n == y) xs

variations n = n:map (*n) [2..6]
problem52 = head . filter (allTheSame . variations) $ [1..]
