takeEnd n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys

str2Int s = read s:: Int

problem48 = str2Int . takeEnd 10 . show . sum . take 1000 $ [ a^a | a<-[1..] ]
