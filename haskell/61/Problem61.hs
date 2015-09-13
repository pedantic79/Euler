import qualified Data.Set as S
import Data.List (foldl')
import qualified Data.Array as A

figurative x n
  | x == 3 = (n*n + n) `div` 2
  | x == 4 =  n*n
  | x == 5 = (3*n*n - n) `div` 2
  | x == 6 =  2*n*n - n
  | x == 7 = (5*n*n - 3*n) `div` 2
  | x == 8 = (3*n*n - 2*n)

back = (`mod` 100)

fig4 = takeWhile ((<10000).snd) . dropWhile ((<999).snd) . figTup
  where
    figTup x = map (\n -> (n, figurative x n)) [1..]
    filt = filter $ \(_,n) -> back n > 9
    

fig4Array x = A.listArray (i, i+l-1) d
  where
    v = fig4 x
    i = fst . head $ v
    d = map snd v
    l = length d

filt rs = filter (not . (`elem` rs) . fst)

maxVal = 6

figArray = A.listArray (3, maxVal) (map fig4Array [3..maxVal])

getSet x
  | x == maxVal = S.fromList $ dig x
  | otherwise = foldl' (flip S.insert) (getSet (x+1)) $ dig x
  where
    dig = map ((`div` 100) . snd) . fig4

getSetArray = A.listArray (3,maxVal) (map getSet [3..maxVal])

f n rs
  | n == maxVal = map (:[]) . filt rs . A.assocs $ figArray A.! n
  | otherwise = [ num:more | num <-  A.assocs $ figArray A.! n
                           , more <- f (n+1) (fst num:rs)
                           , not $ elem (snd num) (map snd more)
                           , S.member (start num) $ getSetArray A.! n
                           ]
  where
    start = (`mod` 100) . snd

problem61 = length $ f 3 []
main = print problem61
