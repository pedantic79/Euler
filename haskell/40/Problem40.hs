import Data.Char (digitToInt)

num2Str :: Integer -> String
num2Str = show


str = take 1000000 . concatMap num2Str $ [1..]

baseline = (str !!) . flip (-) 1

problem40 = product . map (digitToInt . baseline) $ take 7 (iterate (*10) 1)
