import Data.Char (digitToInt)

factorial 1 = 1
factorial m = product [2..m]

problem19 = sum . map digitToInt . show . factorial $ 100

