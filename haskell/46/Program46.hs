import Data.Numbers.Primes (isPrime, primes)

isGoldbach n = or [n == p + 2*s^2 | p <- ps, let s = fn (n - p)]
  where
    ps = takeWhile (<n) primes
    fn = floor . sqrt . fromIntegral . flip div 2

filterNot = filter . (not .)

problem46 = head . filterNot isGoldbach . filterNot isPrime $ [27,29..]
