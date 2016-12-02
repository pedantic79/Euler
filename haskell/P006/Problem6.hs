module P006.Problem6 (diff, problem6) where

sumOfSquares = sum . map (^2)
squareOfSums = (^2) . sum

diff n = squareOfSums [1..n] - sumOfSquares [1..n]

problem6 = diff 100
