figurative x n
  | x == 3 = (n*n + n) `div` 2
  | x == 4 =  n*n
  | x == 5 = (3*n*n - n) `div` 2
  | x == 6 =  2*n*n - n
  | x == 7 = (5*n*n - 3*n) `div` 2
  | x == 8 = (3*n*n - 2*n)

filtFig4 r = filterRange r . fig4
  where 
    fig4 x = takeWhile (<10000) . dropWhile (<999) . map (figurative x) $ [1..]

filterRange r
  | r == 0 = id
  | otherwise = filter (\n -> n > a && n < a+99)
  where
    a = r `mod` 100 * 100

f n r
  | n == 5 = map (:[]) $ filtFig4 r n
  | otherwise = [ num:more | num <- filtFig4 r n
                           , more <- f (n+1) num
                           ]

problem61 = f 3 0                
