import Control.Monad.State
import qualified Data.IntMap as IntMap
import Data.List (unfoldr)

digits :: Int -> [Int]
digits = map (flip (-) (fromEnum '0') . fromEnum) . show

calcSq :: Int -> Int
calcSq = sum . map (\x -> x * x) . digits

next :: Int -> Int
next n
  | n == 1 || n == 89 = n
  | otherwise = next $ calcSq n

terminator :: Int -> State (IntMap.IntMap Int) Int
terminator 1 = return 1
terminator 89 = return 89
terminator n = do
  m <- get
  if IntMap.member n m
    then return (m IntMap.! n)
    else do t <- terminator (calcSq n)
            modify (IntMap.insert n t)
            return t

terminators :: [Int] -> State (IntMap.IntMap Int) [Int]
terminators = mapM terminator


problem92 = length . filter (==89) . map next $ [1..10000000]
problem92a = length . filter (==89) $
             evalState (terminators [1..10000000]) IntMap.empty
