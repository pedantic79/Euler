module Main where
import Criterion.Main
import Data.Char (digitToInt)
import Data.List (unfoldr)

numberToList1 :: Integer -> [Integer]
numberToList1 = map (fromIntegral . digitToInt) . show

numberToList2 :: Integer -> [Integer]
numberToList2 = snd . head . dropWhile ((>0) . fst) . numToListInf

numToListInf n = iterate (\(a,rs) -> (a `quot` 10, a `rem` 10:rs)) (n, [])

numberToList3 :: Integer -> [Integer]
numberToList3 = reverse . unfoldr helper
  where helper x | x == 0    = Nothing
                 | otherwise = let (q, r) = x `quotRem` 10 in Just (r, q)

main = defaultMain
       [ bgroup "show" [ bench "7893" $ nf numberToList1 7893
                       , bench "456 " $ nf numberToList1 456
                       , bench "23  " $ nf numberToList1 23
                       , bench "1   " $ nf numberToList1 1
                       ]
       , bgroup "math" [ bench "7893" $ nf numberToList2 7893
                       , bench "456 " $ nf numberToList2 456
                       , bench "23  " $ nf numberToList2 23
                       , bench "1   " $ nf numberToList2 1
                       ]
       , bgroup "unfd" [ bench "7893" $ nf numberToList3 7893
                       , bench "456 " $ nf numberToList3 456
                       , bench "23  " $ nf numberToList3 23
                       , bench "1   " $ nf numberToList3 1
                       ]
       ]
       
