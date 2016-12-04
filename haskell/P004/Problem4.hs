module P004.Problem4 (maxPalin, products, problem4) where

products l = [a * b | a <- s, b <- s]
  where s = [10^(l-1) .. 10^l-1]

maxPalin = maximum . filter (isPalin . show)
  where isPalin xs = xs == reverse xs

problem4 = maxPalin $ products 3
