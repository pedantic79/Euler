module P009.Problem9 (problem9) where
import Control.Monad (guard)

-- triplet = head [[a,b,c]
--                 | a <- [1..1000]
--                 , b <- [a..1000]
--                 , let c =1000-a-b
--                 , a * a + b * b == c * c
--                 ]

triplet = head $ do
  a <- [1 .. 1000]
  b <- [a .. 1000]
  let c = 1000 - a - b
  guard $ a * a + b * b == c * c
  return [a, b, c]

problem9 = product triplet
