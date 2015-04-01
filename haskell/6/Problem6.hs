sumOfSquares = sum . map (^2)
squareOfSums = (^2) . sum

problem6 = squareOfSums [1..100] - sumOfSquares [1..100]
