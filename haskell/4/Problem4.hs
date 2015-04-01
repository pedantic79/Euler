products = zipWith (*) [100..999] [100..999]
problem4 = maximum $ filter (\x -> show x == reverse (show x)) products
