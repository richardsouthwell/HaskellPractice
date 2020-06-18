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

myProduct :: Ord a => Ord b => Ord c => Ord d => Obj a b -> Obj c d -> Obj (a,c) (b,d)
myProduct (a,f,b) (c,g,d) = (aTimesc, Data.Map.fromList (Prelude.map (\v -> (v, ((f !) (fst v),(g !) (snd v)))) (Data.Set.toList aTimesc)), Data.Set.cartesianProduct b d) where aTimesc = Data.Set.cartesianProduct a c


--(snd v)



--myProduct (a,f,b) (c,g,d) = (((aTimesc, Data.Map.fromList lis, Data.Set.cartesianProduct b d) where lis = Prelude.map ( mymap) (Data.Set.toList aTimesc)) where mymap = (  \v -> (v, ((f !) v,(g !) v))) ) where aTimesc = Data.Set.cartesianProduct a c

xx :: Map Int (Map Int String)
xx = Data.Map.fromList [(1, Data.Map.fromList [(2,"dog"), (3,"cat")])]


-- each map from Obj(a1,a0) to Obj(b1,b0)corresponds to a member of (Map(a1,b1),Map(a0,b0)), and also to a point of b^a
--myExpObj :: Obj(a1,a0) -> Obj(b1,b0) -> Obj((Map(a1,b1),Map(a0,b0)),Map(a0,b0))

-- code to enumerate all such maps
-- add projection maps, intermediate arrows, object validity checker, and arrow validaty checker






-- can make is member setup, so we can properly talk about power sets