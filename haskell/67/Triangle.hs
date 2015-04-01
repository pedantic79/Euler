module Main where
t = [5, 9, 6, 4, 6, 8, 0, 7, 1, 5]
t2 = [[5], [9, 6], [4, 6, 8], [0, 7, 1, 5]]


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

   for foldTri, we calculate the sum of the two rows and return the value
   This let's us compute the next fold
-}                                    
foldTri = foldr1 next

-- Since the fold operations returns a single element array take the head
getMaxFast = head . foldTri

slow f = readFile f >>= print . getMaxSlow . helper
  where helper = map read . words

fast f = readFile f >>= print . getMaxFast . helper
  where helper = map (map read . words) . lines

main = fast "triangle.txt"

