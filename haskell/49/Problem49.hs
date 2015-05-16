import Control.Monad (guard)
import Data.List (sort)
import Data.Numbers.Primes (isPrime)

arePerm a b c =
  let
    fn = sort . show
    a' = fn a
    b' = fn b
    c' = fn c
  in a' == b' && b' == c'

primePerm = do
  i <- [1488..9999-6660]
  let j = i + 3330
  let k = j + 3330
  guard $ isPrime i
  guard $ isPrime j
  guard $ isPrime k
  guard $ arePerm i j k

  return [i,j,k]

problem49 = concatMap show . head $ primePerm
