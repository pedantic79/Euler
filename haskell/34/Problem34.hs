import Data.Maybe (mapMaybe)
import Data.Array ((!),listArray)

factorial n = factArray ! n

factArray = listArray (0, 9) (take 10 fact)
  where fact = 1 : 1 : scanl (*) 2 [3..]

{-
factorial n
  | n < 0     = undefined
  | n < 2     = 1
  | otherwise = product [2..n]
-}

digits = map (flip (-) (fromEnum '0') . fromEnum) . show

digitsFactorial = map factorial . digits

igFactSum n
  | s == n    = Just n
  | otherwise = Nothing
  where s = sum (digitsFactorial n)

problem34 = sum . mapMaybe digFactSum $ [3..2540160]
