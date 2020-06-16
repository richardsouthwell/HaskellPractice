
-- ----------------- product type constructor

data ProductTC a b = ProductDC a b deriving (Show)
proj1 :: ProductTC a b -> a 
proj1 (ProductDC x _) = x
proj2 :: ProductTC a b -> b 
proj2 (ProductDC _ y) = y

testResult = proj1 (ProductDC 7 9)

-- ----------------- Fibonacci list practice


myLength :: [a] -> Int
myLength [] = 0
myLength s = (+) 1 (myLength (tail s))

attachNextFib :: [Int] -> [Int]
attachNextFib s = if ((myLength s) < 2) then s else s ++ [(s !! ((myLength s) - 2)) + (s !! ((myLength s) - 1))]

myIterate :: a -> (a -> a) -> Int -> a
myIterate x f 0 = x
myIterate x f n = f (myIterate x f (n - 1))

myFibs = myIterate [1,1] attachNextFib 5

-- ----------------- Coproduct functor + safe square root iteration


data MaybeTC a = MyNothing | MyJust a deriving (Show)

-- MyNothing and MyJust are just injections from A to 1+A

-- MaybeTC tells how to map objects of Set to objects of Set, better describe what happens to arrows too ...

myFmap :: (a -> b) -> MaybeTC a -> MaybeTC b
myFmap _ MyNothing = MyNothing
myFmap f (MyJust x) = (MyJust (f x)) 

-- make instance of the functor type class

instance Functor MaybeTC where  
    fmap f (MyJust x) = MyJust (f x)  
    fmap f MyNothing = MyNothing  

-- MaybeTC is a functor from Set to Set, which sends an object A to A+1, and sends a function to itself plus 
-- MyNothing to MyNothing

addOne :: Double -> Double
addOne x = x + 1

-- fmap addOne (MyJust 7)


safeSqrt :: Double -> MaybeTC Double
safeSqrt x = if (x < 0) then MyNothing else (MyJust (sqrt x))
myFunctorTest = fmap addOne (safeSqrt 36.0)


-- ----------------- Product functor 

data PairWithStringTC a = PairWithStringDC a String deriving(Show)

instance Functor PairWithStringTC where  
    fmap f (PairWithStringDC x s) = PairWithStringDC (f x) s  


myFunctorTest2 = fmap addOne (PairWithStringDC 7.0 "my name")


-- add intermediary arrows
-- natural transformations ?
-- graphs and trees

--data SquaringTC a = SquaringDC a a deriving (Show)
--instance Functor SquaringTC where 
--     fmap f (SquaringDC x x) = SquaringDC (f x) (f x)  

--fmapSq :: (a -> b) ->  SquaringTC a -> SquaringTC b
--fmapSq f (SquaringDC x x) = SquaringDC (f x) (f x)


--class MyContravariant f where
--    myContramap :: (b -> a) -> f a -> f b

--type Graph a = (SquaringDC a a) -> Bool
--instance MyContravariant Graph where
--    myContramap g r = r . (fmapSq g)

-- data ProductTC a b = ProductDC a b deriving (Show)
--instance Functor MaybeTC where  

--instance Functor MaybeTC where  
--    fmap f (MyJust x) = MyJust (f x)  
--    fmap f MyNothing = MyNothing  

--type Subset a = a -> Bool
--instance MyContravariant Subset where
--   myContramap g r = r . g

--instance MyContravariant (-> Bool) where
 --   myContramap = flip (.)

--class Contravariant f where
-- contramap :: (a0 -> a1) -> f a1 -> f a0

--instance Contravariant (-> Bool) where
--    contramap = flip (.)

--newtype Op z a =
--  Op { getOp :: a -> z }

--instance Contravariant (Op a) where
--  contramap f g = Op (getOp g . f)
  --contramap f g = (g . f)

--class Contravariant (f :: * -> *) where
 -- contramap :: (a -> b) -> f b -> f a

--type RReader r a = r -> a

--instance Functor (RReader r) = r -> a where 
 --   fmap f g = f . g

type Subset a = a -> Bool

mm :: (a -> b) -> Subset b -> Subset a
mm trans inpu = (\x -> (inpu (trans x)))

vv :: Int
vv = 7

hh :: Subset Int
hh = (\y -> ((mod y 2) == 0))

class MyContravariant f where
 myContramap :: (a -> b) -> f b -> f a

--instance MyContravariant Subset where
-- myContramap trans inpu = (\x -> (inpu (trans x)))

data SubsetD a = SomeConstructor ( a -> Bool)

instance MyContravariant SubsetD where
  myContramap trans (SomeConstructor inpu) = SomeConstructor (\x -> (inpu (trans x)))

myContramapSub :: (a -> b) -> SubsetD b -> SubsetD a

myContramapSub trans (SomeConstructor inpu) = SomeConstructor (\x -> (inpu (trans x)))

extractFunction :: SubsetD a -> ( a -> Bool)
extractFunction (SomeConstructor fun) = fun

isEven :: Int -> Bool
isEven = (\y -> ((mod y 2) == 0))

memberOfType = SomeConstructor isEven

stringLength :: String -> Int 
stringLength s = length s

newMember = myContramap stringLength memberOfType

gotOut = extractFunction newMember

resultTest1 = gotOut "door"

-- next .. define squaring functor, then compose to get graph functor

data SquareTC a = SquareDC a a deriving(Show)
instance Functor SquareTC where
    fmap g (SquareDC x y) = SquareDC (g x) (g y) 

p1Sq :: SquareTC a -> a
p1Sq (SquareDC x y) = x
p2Sq :: SquareTC a -> a
p2Sq (SquareDC x y) = y

fmapSq :: (a -> b) -> (SquareTC a) -> (SquareTC b)
fmapSq g (SquareDC x y) = SquareDC (g x) (g y) 

--myContramapSub :: (a -> b) -> SubsetD b -> SubsetD a
--myContramapSub trans (SomeConstructor inpu) = SomeConstructor (\x -> (inpu (trans x)))

data Graph a = GraphConstructor ( (SquareTC a) -> Bool)
myContramapGraph :: (a -> b) -> Graph b -> Graph a
myContramapGraph g (GraphConstructor r) = GraphConstructor (\v -> (r (SquareDC (g (p1Sq v)) (g (p2Sq v)))))

instance MyContravariant Graph where
  myContramap g (GraphConstructor r) = GraphConstructor (\v -> (r (SquareDC (g (p1Sq v)) (g (p2Sq v)))))

extractGraphMap :: Graph a -> ((SquareTC a) -> Bool)
extractGraphMap (GraphConstructor r) = r

divisMap :: (SquareTC Int) -> Bool
divisMap (SquareDC x y) = (0 == (mod x y))

myBigGraph = GraphConstructor divisMap
newGraph = myContramap stringLength myBigGraph
gotOutGraph = extractGraphMap newGraph
resultTest2 = gotOutGraph (SquareDC "door" "it")






 

--g :: a -> b
--r:: (SquareTC a) -> Bool

-- ------------------------

class MyFunctor f where 
    myfmap :: (a -> b) -> f a -> f b


-- implement A^2 functor
-- demonstrate it is equivalant to squaring functor
-- clip together with subset functor for control

data PairInTC a = PairInDC (Bool -> a)
instance MyFunctor PairInTC where
  myfmap g (PairInDC r) = (PairInDC (\x -> (g (r x))))

twoWords :: Bool -> String
twoWords False = "bull"
twoWords True = "elephant"

twoNumbers = myfmap stringLength (PairInDC twoWords)
extractPair :: (PairInTC a) -> (Bool -> a)
extractPair (PairInDC s) = s
twoNumbersResult = extractPair twoNumbers
myResult = twoNumbersResult True

--finiteContainsQ :: (Int -> Bool) -> (Int -> Bool) -> Int -> Bool

--finiteContainsQ x y n = map k [0..(n-1)] where k i = (bb (x i) (y i) ) where bb

impliesMicro :: Bool -> Bool -> Bool
impliesMicro True True = True
impliesMicro False True = True
impliesMicro False False = True
impliesMicro True False = False

finiteContainsQ :: (Int -> Bool) -> (Int -> Bool) -> Int -> Bool
finiteContainsQ x y n = (map (\i -> (impliesMicro (x i) (y i) )) [0..(n-1)]) == take n (repeat True)

binaryToFiniteBoolSequence :: [Int] -> (Int, (Int -> Bool))
binaryToFiniteBoolSequence bin = (length bin, (\i -> ((bin !! i) == 1) ))

containsBinStringQ :: [Int] -> [Int] -> Bool
containsBinStringQ bin1 bin2 = finiteContainsQ (snd (binaryToFiniteBoolSequence bin1))
 (snd (binaryToFiniteBoolSequence bin2)) (fst (binaryToFiniteBoolSequence bin1))

myTest = containsBinStringQ [1,0,0] [1,0,1] 

mapToPairsIsh :: Int -> (Int -> Int) -> Int -> ([(Int,Int)], Int)
mapToPairsIsh numL mapping numR =  (map (\v -> (v, mapping v)) [1..numL], numR)


pairsToMapIsh :: [(Int,Int)] -> Int -> (Int, (Int -> Int),  Int)
pairsToMapIsh maplist numR = (length maplist, (
  \v -> if (v > 0 && v <= (length maplist)) then (snd (maplist !! (v + 1))) else 0), numR )


t1 :: (a, b, c) -> a
t1 (x, y, z) = x
t2 :: (a, b, c) -> b
t2 (x, y, z) = y
t3 :: (a, b, c) -> c
t3 (x, y, z) = z

--  go x = v + v where v = x + 1

mapToPairs :: (Int, (Int -> Int),  Int) -> ([(Int,Int)], Int)
mapToPairs info = mapToPairsIsh (t1 info) (t2 info) (t3 info) 

pairsToMap :: ([(Int,Int)], Int) -> (Int, (Int -> Int),  Int)
pairsToMap info = pairsToMapIsh (fst info) (snd info)

-- [a | a <- [1,2,3], a > 1]


--finiteContainsQ x y n = map k [0..(n-1)] where k i = (bb (x i) (y i) ) where bb


-- get graph of containment, point it out in category of graphs, model poset of containment
-- model categories of functions via capped integers
