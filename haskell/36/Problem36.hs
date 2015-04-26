import Data.Char (intToDigit)
import Numeric (showIntAtBase)

convertToBase b n
  | b == 10   = show n
  | otherwise = showIntAtBase b intToDigit n ""

checkPalindrome s = s == reverse s

checkBase10And2 n = checkBase10 n && checkBase2 n
  where
    checkBase10 = checkPalindrome . convertToBase 10
    checkBase2  = checkPalindrome . convertToBase 2


problem36 = sum . filter checkBase10And2 $ [1..999999]
