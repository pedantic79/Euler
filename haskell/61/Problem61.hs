import qualified Data.List as L
import qualified Data.Array as A

figurative x n
  | x == 3 = (n*n + n) `div` 2
  | x == 4 =  n*n
  | x == 5 = (3*n*n - n) `div` 2
  | x == 6 =  2*n*n - n
  | x == 7 = (5*n*n - 3*n) `div` 2
  | x == 8 =  3*n*n - 2*n

figValid x
  | x == 3 = [45..99] ++ [102..140]
  | x == 4 = [32..99]
  | x == 5 = [26..33] ++ [35..81]
  | x == 6 = [23..50] ++ [52..70]
  | x == 7 = [21..40] ++ [42..63]
  | x == 8 = [19..58]

fig4Dig x = map (\n -> (x, figurative x n)) $ figValid x

front = (`div` 100)
back = (`mod` 100)

figData = A.listArray (10,99) v
  where
    v = L.groupBy g . L.sortBy s . concatMap fig4Dig $ [3..8]
      where
        g (_,a) (_,b) = front a == front b
        s (_,a) (_,b) = compare a b

-- figData = M.fromList $ map (\l -> (front . snd $ head l, l)) v

getNext x rs
  | x < 10 = []
  | otherwise = filter (\(n,_) -> notElem n rs) $ figData A.! x

q f x rs
  | null next = [[f]]
  | otherwise = [ val:more | (num,val) <- next
                           , more <- q f (back val) (num:rs)
                           ]
  where next = getNext x rs

-- 28684
problem61 = sum . head . filter filt .
            filter (\l -> length l == 6) .
            concatMap (\(n,v) -> q v (back v) [n]) $ fig4Dig 3
  where
    filt l = (front . last $ l) == (back . last . init $ l)
