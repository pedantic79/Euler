import Data.Char (ord, toUpper)
import Data.List (sort)
import Data.Text (pack, splitOn, unpack)

charVal ch = (ord . toUpper) ch - 64
nameVal = sum . map charVal


parse str = map reverse $ parse' str [] []
  where parse' (s:ss) acc all | testIgn s = parse' ss acc all
                              | testCom s = parse' ss [] (acc:all)
                              | otherwise = parse' ss (s:acc) all
        parse' [] ss all = ss:all
        testIgn s = s == '"' || s == '\\' || s == ' '
        testCom s = s == ','

-- Alternative parse with Data.Text
parse2 = map ((read :: String -> String) . unpack) . splitOn comma . pack
  where comma = pack ","

valueList = map nameVal . sort . parse
valueListMult str = zipWith (*) [1..] (valueList str)

problem22 = readFile "p022_names.txt" >>= print . sum . valueListMult


