import qualified Data.Set as S

p n = n * (3 * n - 1) `div` 2

penta = map p [1..2500]
pentaSet = S.fromList penta
isPenta = flip S.member pentaSet

solutions = [ a - b
            | a <- penta
            , b <- takeWhile (<a) penta
            , isPenta (a+b)
            , isPenta (a-b)
            ]

problem44 = head solutions
                   
                  

