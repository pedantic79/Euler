module P001.Problem1 (mult3or5, problem1) where

mult3or5 n = sum [ x | x <- [1..(n-1)]
                     , x `mod` 3 == 0 || x `mod` 5 == 0 ]

problem1 = mult3or5 1000
