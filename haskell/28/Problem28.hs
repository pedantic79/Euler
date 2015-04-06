{-

43 44 45 46 47 48 49
42 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 28
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31
-}

snd3 (_,y,_) = y
rowStp = map (\x -> ((x - 2)^2+1, x^2, x-2)) [1,3..]
rowMax = map snd3 rowStp

range n = head . dropWhile ((<n) . snd3) $ rowStp

includeList 1 = [1]
includeList n = incList
  where (a, b, c) = range n
        incList = tail [a-1, a+c .. b]

incListSum = sum . includeList

problem28 = sum . map incListSum  $ takeWhile (<=1001^2) rowMax
