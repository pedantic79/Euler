import Data.Set (fromList,member)
import Data.Char (ord,toUpper)


t n = n * (n + 1) `div` 2

tMap = fromList . map t  $ [1..26]

char2Val = (+1) . flip (-) (ord 'A') . ord . toUpper

str2Val = sum . map char2Val

isTriNum = (`member` tMap) . str2Val

problem42 = do
  l <- readFile "p042_words.txt"
  let ws = read $ "[" ++ l ++ "]"
  print . length . filter isTriNum $ ws
