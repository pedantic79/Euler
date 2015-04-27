{-

Here is a 5x5 square, the numbers represent the ways to get to that point.
The value is the sum of the two lines that go to it.  On the edges it is
one.  

 1--1--1--1--1
 |  |  |  |  |
 1--2--3--4--5
 |  |  |  |  |
 1--3--6-10-15
 |  |  |  |  |
 1--4-10-20-35
 |  |  |  |  |
 1--5-15-35-70

We can take the diagonal values.  The first is [1].
After that it is [1, 1] then [1, 2, 1].
So after 5 steps, the list will be [1, 4, 6, 4, 1].
This is Pascal's Triangle.


From that point we can start reducing, by simply not
adding the 1's at the beginning and end.
[1, 4, 6, 4, 1] -> [5, 10, 10, 5] -> [15, 20, 15]

Alternatively, rather than reducing we could continue
to create additional values. Notice that the middle
values 0th, 2nd, 4th, 10th is the answer. For 0,
1, 2, and 4 respectively.

-}



incPaths xs = 1:incPaths' xs
  where incPaths' (b:[]) = [1]
        incPaths' (a:b:bs) = a + b : incPaths' (b:bs)

decPaths (a:b:bs) = a + b : decPaths (b:bs)
decPaths (b:[]) = []
decPaths [] = []

-- "normal" way to implement pascal's triange
pascal = iterate (\row -> zipWith (+) (0:row) (row ++ [0])) [1]
inc = iterate incPaths [1]
dec = takeWhile (not . null) . iterate decPaths
solution2 n = inc !! (2*n) !! n
solution1 = head . last . dec . (!!) inc

problem15 = solution1 20
