import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (group,sort)

tri = do
  a <- [1..1000]
  b <- [a..1000]
  let d = a^2 + b^2
  let c = floor . sqrt . fromIntegral $ d
  guard $ d == c^2

  let p = a + b + c
  guard $ p <= 1000

  return p

problem39 = snd . maximum . map (length &&& head) . group . sort  $ tri
