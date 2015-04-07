import Data.Array ((!),listArray)
import Data.Char (digitToInt)

numToNumList :: Integer -> [Integer]
numToNumList = map (fromIntegral . digitToInt) . show

fifthPowers = listArray (0, 9) (0:1:map (^5) [2..9])

sumDigits = foldl (\a x -> (fifthPowers ! x) + a) 0 . numToNumList
sumDigits2 = sum . map (\x -> x^5) . numToNumList

problem30 =  sum . filter (\n -> n == sumDigits n) $ [2..(9^5)*6]
