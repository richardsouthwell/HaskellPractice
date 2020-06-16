import Data.Set
import Data.Map

type Obj a b = (Set a, Map a b, Set b)

ex1 :: Obj String Int
ex1 = (Data.Set.fromList ["a", "b", "c"], Data.Map.fromList [("a",1),("b",1),("c", 2)], Data.Set.fromList [1,2,3])
t1 :: (a,b,c) -> a
t1 (x,y,z) = x
t2 :: (a,b,c) -> b
t2 (x,y,z) = y
t3 :: (a,b,c) -> c
t3 (x,y,z) = z

ex2 = ((t2 ex1) !) "b"