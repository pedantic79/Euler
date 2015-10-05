import qualified Data.List as L
import Control.Monad (ap)

cubeList' = map (^3) [1..]

cubeList l m = takeWhile (len (<=m)) . dropWhile (len (<l)) $ cubeList'
  where len p = p . length . show

groupNumVal = ap (,) (L.sort . show)

grpSnd = L.groupBy (\(_,x) (_,y) -> x == y) . L.sortBy (\(_,x) (_,y) -> compare x y)

-- 127035954683
problem62 = L.minimum . map fst .  concat . f 5 . grpSnd . map groupNumVal $ cubeList 1 12
  where
    f x = filter (\n -> length n == x)
