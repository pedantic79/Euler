import qualified Data.List as L
import qualified Data.Set as S

permuteInt :: Integer -> [Integer]
permuteInt = uniq . map read . filter (\n -> head n /= '0') .  L.permutations . show

p = permuteInt 41063625

uniq = S.elems . S.fromList

cubes = S.fromList . map (\x -> x*x*x) $ [1..100000]

example = filter (`S.member` cubes) p
