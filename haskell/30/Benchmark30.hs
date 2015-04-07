module Main where
import Criterion.Main
import Data.Char (digitToInt)

numberToList1 :: Integer -> [Integer]
numberToList1 = map (fromIntegral . digitToInt) . show

numberToList2 :: Integer -> [Integer]
numberToList2 n = snd . head . dropWhile ((>0) . fst) $
                  iterate (\(a,rs) -> (a `quot` 10, a `rem` 10:rs)) (n, [])


main = defaultMain
       [ bgroup "show" [ bench "7893" $ nf numberToList1 7893
                       , bench "456 " $ nf numberToList1 456
                       , bench "23  " $ nf numberToList1 23
                       , bench "1   " $ nf numberToList1 1
                       ]
       , bgroup "math" [ bench "7893" $ nf numberToList2 7893
                       , bench "456" $ nf numberToList2 456
                       , bench "23" $ nf numberToList2 23
                       , bench "1" $ nf numberToList2 1
                       ]
       ]
     
