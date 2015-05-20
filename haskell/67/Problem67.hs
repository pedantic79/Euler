import Control.Monad (liftM)

parseFile fp = liftM parse . readFile $ fp
  where
    parse s = do
      w <- liftM words . lines $ s
      return $ fmap read w

solve = head . foldr1 next
  where
    next _ [_] = []
    next (x:xs) (y:z:zs) = x + max y z : next xs (z:zs)          

main = print =<< liftM solve (parseFile "triangle-yodle.txt")



