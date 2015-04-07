module Main where
import Criterion.Main
import Data.List (union,nub)

generate1 :: Int -> Int -> [Int]
generate1 n a = map (p a) [2..n]
  where p a b = a ^ b

generate2 :: Int -> Int -> [Int]
generate2 n a = take (n-1) . iterate (*a) $ (a*a)

l1 a b = map (generate2 a) [2..b]
l2 a b = concatMap (generate2 a) [2..b]
bmUnn n = length . foldr1 (union) $ l1 n n
bmNub n = length . nub $ l2 n n

main = defaultMain
       [ bgroup "map 35" [ bench "025" $ nf (generate1 35) 25
                         , bench "050" $ nf (generate1 35) 50
                         , bench "075" $ nf (generate1 35) 75
                         , bench "100" $ nf (generate1 35) 100
                         ]
       , bgroup "itr 35" [ bench "025" $ nf (generate2 35) 25
                         , bench "050" $ nf (generate2 35) 50
                         , bench "075" $ nf (generate2 35) 75
                         , bench "100" $ nf (generate2 35) 100
                         ]
       , bgroup "flpMap" [ bench "025" $ nf ((flip generate1) 35) 25
                         , bench "050" $ nf ((flip generate1) 35) 50
                         , bench "075" $ nf ((flip generate1) 35) 75
                         , bench "100" $ nf ((flip generate1) 35) 100
                         ]
       , bgroup "flpItr" [ bench "025" $ nf ((flip generate2) 35) 25
                         , bench "050" $ nf ((flip generate2) 35) 50
                         , bench "075" $ nf ((flip generate2) 35) 75
                         , bench "100" $ nf ((flip generate2) 35) 100
                         ]
       , bgroup "unn" [ bench "2" $ whnf bmUnn 2
                      , bench "4" $ whnf bmUnn 4
                      , bench "6" $ whnf bmUnn 6
                      , bench "8" $ whnf bmUnn 8
                      ]
       , bgroup "nub" [ bench "2" $ whnf bmNub 2
                      , bench "4" $ whnf bmNub 4
                      , bench "6" $ whnf bmNub 6
                      , bench "8" $ whnf bmNub 8
                      ]
         
       ]
         
