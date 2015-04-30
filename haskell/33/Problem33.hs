import Control.Monad (guard)
import Data.Ratio ((%),denominator)

fraction = do
  s <- [1..9]
  a <- [1..9]
  b <- [a..9]

  guard $ a % b == (a * 10 + s) % (s * 10 + b)
  return $ a % b


-- [(16,64),(26,65),(19,95),(49,98)]

problem33 = denominator . product $ fraction
