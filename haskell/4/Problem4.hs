products = [a * b | a <- [100..999], b <- [100..999]]
problem4 = maximum $ filter (\x -> show x == reverse (show x)) products
