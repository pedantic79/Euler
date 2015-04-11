import Data.List (group,sort)

-- The values of the coins
coins = [200, 100, 50, 20, 10, 5, 2, 1]
currency = reverse coins

-- Calculate the total value based on the coin count
value = sum . zipWith (*) coins

-- All the possible combinations excluding 2p and 1p
-- We only care how much they add up to. There are 284130 combinations.
q = [value [a,b,c,d,e,f] | a<-[0..1], b<-[0..2], c<-[0..4],
               d<-[0..10],e<-[0..20],f<-[0..40]]

-- This is the count of coins that already equal 200
countA = length . filter (== 200) $  q

-- List containing the (value, count) of all the q combinations that
-- were less than 200. The fst value represents the value of those
-- combinations, and the snd value represents the number of ways to
-- get to the fst value.
countSmall = map (\a@(x:_) -> (x, length a)) . group . sort $ filter (<200) q


-- Given the starting value, we find how much value we need to reach 200.
-- 200 = starting value + 2p*y + 1p*(how over much is required)
-- This means to find y, we only need to take 200 - starting value `div` 2
-- Then there is one more case where y is 0.
numbers = map (\(a,b) -> ((200-a) `div` 2 + 1,b)) countSmall

-- We multiply the tuples together since the count b represents the number
-- of 5p-200p combinations, and a represents the number of 1p-2p combinations
-- to get to 200.
countB =  sum $ map (\(a,b) -> a * (fromIntegral b)) numbers

problem31 = countA + countB
