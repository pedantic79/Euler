import Data.List (sort)


checkIs19PanDigit = (== "123456789") . sort

num2Str :: Integer -> String
num2Str = show

str2Num :: String -> Integer
str2Num = read

generateNum = takeWholeNum . gen


gen n = map ((\x -> (length x, x)) . num2Str . (*n)) $ [1..]

-- take only whole numbers.
takeWholeNum = helper (0,"")
  where helper (a1,a2) ((v1,v2):vs)
          | a1 >= 9 = a2
          | otherwise = helper (a1+v1, a2++v2) vs

problem38 = maximum .
            map str2Num .
            filter checkIs19PanDigit .
            map generateNum $ [1..9876]
