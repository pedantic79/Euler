module Main where
t = [5, 9, 6, 4, 6, 8, 0, 7, 1, 5]
t2 = [[5], [9, 6], [4, 6, 8], [0, 7, 1, 5]]


getMaxSlow :: [Integer] -> Integer
getMaxSlow = getMax' 0 0
  where getMax' pos row tri | null nextTri = value
                            | otherwise = value + max left right
          where
            left = getMax' pos nextRow nextTri
            right = getMax' (pos + 1) nextRow nextTri
            nextRow = row + 1
            value = tri !! pos
            nextTri = drop nextRow tri

{- Add the row (x:xs) with the next row (y:z:zs)
   Return the sum.
   e.g. next [4, 6, 8] [0, 7, 1, 5]
   returns [11, 13, 13]
-}
next :: [Integer] -> [Integer] -> [Integer]
next _ [z]           = []
next (x:xs) (y:z:zs) = x + max y z : next xs (z:zs)

{- foldr1 starts by passing the last two elements of the list to the function
   it then calls the function again with the next element and value returned by
   the previous call. e.g.

   foldr1 (+) [1, 2, 3, 4] = (+) 3 4
                           = (+) 2 7
                           = (+) 1 9
                           = 10

   in otherwords: foldr1 (+) [1, 2, 3, 4]    = 10
                  1 + foldr1 (+) [2, 3, 4]   = 1 + 9
                  1 + 2 + foldr1 (+) [3, 4]  = 1 + 2 + 7
                  1 + 2 + 3 + foldr1 (+) [4] = 1 + 2 + 3 + 4

   for getMaxFast, we use foldr1 to calculate the sums of the last row moving
   right to left. At the end of the foldr1, we have a single element list
   containing the value. Use head to take that.
-}
getMaxFast :: [[Integer]] -> Integer
getMaxFast = head . foldr1 next

getMax algo mapper f = print . algo . mapper =<< readFile f

slow :: FilePath -> IO ()
slow = getMax getMaxSlow (map read . words)

fast :: FilePath -> IO ()
fast = getMax getMaxFast (map (map read . words) . lines)

main :: IO ()
main = fast "triangle.txt"

