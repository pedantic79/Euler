import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

slice from to xs = take (to - from + 1) (drop from xs)
look m = fromMaybe "" $ lookup m numStr
lookStr s = look (read s :: Int)
gen n = map (\(a,b) -> (n+a,(look n)++b)) $ slice 1 9 numStr

numStr = [ (0,"zero"), (1,"one"), (2,"two"), (3,"three"), (4,"four")
         , (5,"five"), (6,"six"), (7,"seven"), (8,"eight"), (9,"nine")
         , (10,"ten"), (11,"eleven"), (12,"twelve"), (13,"thirteen")
         , (14,"fourteen"), (15,"fifteen"), (16,"sixteen")
         , (17,"seventeen"), (18,"eighteen"), (19,"nineteen")
         , (20,"twenty")] ++ gen 20 ++
         [ (30,"thirty")] ++ gen 30 ++
         [ (40,"forty")] ++ gen 40 ++
         [ (50,"fifty")] ++ gen 50 ++
         [ (60,"sixty")] ++ gen 60 ++
         [ (70,"seventy")] ++ gen 70 ++
         [ (80,"eighty")] ++ gen 80 ++
         [ (90,"ninety")] ++ gen 90


toEnglish :: String -> String
toEnglish "1000" = "onethousand"
toEnglish (o:[]) = lookStr [o]
toEnglish (t:o:[]) = lookStr [t,o]
toEnglish (h:"00") = lookStr [h] ++ "hundred"
toEnglish (h:t:o:[]) = lookStr [h] ++ "hundredand" ++ lookStr [t,o]

problem17 = sum . map (length . toEnglish . show) $ [1..1000] 


                                                  
