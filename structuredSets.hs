import Data.Set
import Data.Map

type DS a = (a -> a)
-- we coul use this approach if we could use finite types, and check equality of functions
--isArrow :: (a -> b) ->  (a -> a) -> (b -> b) -> Bool
--isArrow f u1 u2 = ((f . u1) == (u2. f))

prodDS :: (a -> a) -> (b -> b) -> (a,b) -> (a,b)
prodDS u1 u2 (x1, x2) = (u1 x1, u2 x2)

-- addition

-- can we also program coproducts and coEqualizers

-- use Map representation

-- experiment with product

-- try using map to represent dynamical systems

-- see if we can get at the important limits + colimits

-- print results to external file, and use mathematica

-- do same ideas for category of functions

-- get finite types representation reapproach, and idris reapproach

-- use Bell approach to Set^(finite_poset)

--type DS a = Map a a

exDS1 :: Map Int Int
exDS1 =  Data.Map.fromList [(1,2), (5,3)]

-- check if an endomap corresponds to a proper dynamical system (if outputs are contained in inputs)
isDSQ :: Ord a => Map a a -> Bool
isDSQ s = (isSubsetOf (Data.Set.fromList (Prelude.map snd lis)) (Data.Set.fromList (Prelude.map fst lis))) where lis = Data.Map.toList s

-- if Ord is only required for isSubset then use custom function instead


gg1 :: Num b => Num a => a -> b -> (a, b) 
gg1 x y = (x + x, y + y)

-- isArrowQ :: Ord a => Ord b => Map a b -> Map a a -> Map b b -> Bool

getSet :: Ord a => Map a a -> Set a 
getSet m = Data.Set.fromList (Prelude.map fst (Data.Map.toList m)) 

-- looks like pairs of integers are also members of Ord, that is good

--isArrowQ :: f u1 u2 = 

myOrd :: Ord a => (a,a) -> (a,a) -> Bool
myOrd (x1, x2) (y1, y2) = ((x1, x2) < (y1, y2))

mapProduct :: Ord a => Ord b => Map a a -> Map b b -> Map (a,b) (a,b)
mapProduct m n = Data.Map.fromList (Prelude.map ff (Data.Set.toList (cartesianProduct (getSet m) (
    getSet n)))) where ff = (\v -> (v, (m ! (fst v), n ! (snd v))) ) 

twoCycle :: Map Int Int
twoCycle = Data.Map.fromList [(0,1),(1,0)]
threeCycle :: Map Int Int
threeCycle = Data.Map.fromList [(0,1),(1,2),(2,0)]
cycleProduct = mapProduct twoCycle threeCycle

-- get projection maps, and intermediary arrows

-- to add (map a a) and (map b b) make them both into map (a,b) (a,b) form, by multiplying with single 
-- elements of one another (that way preserving ording), also include 

--normalize :: (Map a a, Map b b) -> (Map (a, b), (a, b))

-- add representative unit to carry around for the purpose of eing able to form general coproducts

-- coproduct is in (a,b,Bool), for discriminated union



-- Prelude.map (Data.Map.fromList [("a",2), ("b",3)] !) ["a","b"] 



--myOrd2 :: Ord a => Ord b => Either a b -> Bool
--myOrd 


type Obj a = (Map a a, a)

myProduct :: Ord a => Ord b => Obj a -> Obj b -> Obj (a, b)
myProduct (m, x) (n, y) = ((mapProduct m n), (x,y))

--myCoproduct :: Ord a => Ord b => Obj a -> Obj b -> Obj (a, b, Bool)

-- product a with b's representative, and with 0
-- product b with a's representative, and with 1
-- do map union

--massageRight :: Ord a => Ord b => Obj a -> b -> Bool -> Obj (a,b,Bool)
--massageRight (m, x) y b = ((Data.Map.fromList q), (x,y,b)) where q = Prelude.map (\v -> (((fst v), y, b), ((snd v), y, b))) (Data.Map.toList m) 

massageRight :: Ord a => Ord b => Obj a -> b -> Bool -> Map (a,b,Bool) (a,b,Bool)
massageRight (m, x) y b = (Data.Map.fromList q) where q = Prelude.map (\v -> (((fst v), y, b), ((snd v), y, b))) (Data.Map.toList m) 


--massageLeft :: Ord a => Ord b => Obj a -> b -> Bool -> Obj (b,a,Bool)
--massageLeft (m, x) y b = ((Data.Map.fromList q), (y,x,b)) where q = Prelude.map (\v -> (( y, (fst v), b), (y, (snd v), b))) (Data.Map.toList m) 

massageLeft :: Ord a => Ord b => Obj a -> b -> Bool -> Map (b,a,Bool) (b,a,Bool)
massageLeft (m, x) y b = (Data.Map.fromList q) where q = Prelude.map (\v -> (( y, (fst v), b), (y, (snd v), b))) (Data.Map.toList m) 

myCoproduct :: Ord a => Ord b => Obj a -> Obj b -> Obj (a, b, Bool)
myCoproduct (m, x) (n, y) = (Data.Map.union (massageRight (m, x) y False) (massageLeft (n, y) x True), (x,y,False))

-- --------------

myEqualizer :: Ord a => Ord b => Obj a -> (a -> b) -> (a -> b) -> (Obj a, Map a a)
myEqualizer (m,x) f g = ((Data.Map.fromList myList, x), Data.Map.fromList (Prelude.map (\v -> (fst v, fst v)) myList)) where myList = [a | a <- (Data.Map.toList m), f (fst a) == g (fst a)]

