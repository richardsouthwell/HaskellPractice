import Data.Set
import Data.Map

quickSort :: [Int] -> [Int]
quickSort [] = []
-- quickSort g = g
quickSort (x : xs) = (quickSort [a | a <- xs, a <= x]) ++ [x] ++ (quickSort [a | a <- xs, a > x])
exQuickSort = quickSort [5,8,2,8,1,2,9]

ex1 = head [1,2,3]
ex2 = tail [1,2,3]
ex3 = [1,2,3] !! 1
ex4 = Prelude.take 2 [1,2,3]
ex5 = Prelude.drop 1 [1,2,3]
ex6 = length [1,2,3]
ex7 = sum [1,2,3]
ex8 = ((Prelude.take 3 xs) ++ (Prelude.drop 3 xs)) where xs = [1..7]

myTake :: Int -> [a] -> [a]
myTake 0 u = []
myTake n [] = []
myTake n (x: xs) = x : (myTake (n - 1) xs) 

hh :: Set Int
hh = Data.Set.fromList [1,2]
hj = Data.Set.fromList [2,3]
hh2 = Data.Set.intersection hh hj
ex9 = (Data.Set.fromList [1,2] == Data.Set.fromList [2,1]) 

map1 = Data.Map.fromList [(1,2), (5,3)]
map2 = Data.Map.fromList [(5,3), (1,2)]
ex10 = (2 == (map1 ! 1))

map3 = Data.Map.fromList [("a",2), ("b",3)]
map4 = Data.Map.fromList [(2,3), (3,7)]
map5 = Data.Map.map (map4 !) map3
-- composition
-- map (f !) g

type Obj a b = (Map a b, Set b)

-- use contains ? to test if object is legit

