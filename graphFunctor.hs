-- Goal: represent a directed graph on vertex set A by describing its edge set as a subset of A times A
-- First describe contravariant functors
class MyContravariant f where
 myContramap :: (a -> b) -> f b -> f a

-- make functor that sends set A to the set of subsets of A (where subsets correspond to points of Bool^A)
data SubsetTC a = SubsetDC (a -> Bool)
instance MyContravariant SubsetTC where
  myContramap trans (SubsetDC inpu) = SubsetDC (\x -> (inpu (trans x)))
-- contramap pullsback the subset (inpu :: b -> Bool) of b along (transs :: a -> b)

-- for example, we pullback the set of even integers, along stringlength, to get even length strings..
isEven :: SubsetTC Int
isEven = SubsetDC (\y -> ((mod y 2) == 0))
stringLength :: String -> Int 
stringLength s = length s
isStringEven = myContramap stringLength isEven
extractFunction :: SubsetTC a -> ( a -> Bool)
extractFunction (SubsetDC fun) = fun
testResult1 = (extractFunction isStringEven) "door"
--Question: is there a way to represent Sub(A) ? That is, can we represent how subsets contain each other ?

-- describe covariant functors
class MyFunctor f where 
    myfmap :: (a -> b) -> f a -> f b

-- make "squaring" functor that sends set A to (A times A)
data SquareTC a = SquareDC a a deriving(Show)
instance MyFunctor SquareTC where
    myfmap g (SquareDC x y) = SquareDC (g x) (g y) 
p1 :: SquareTC a -> a
p1 (SquareDC x y) = x
p2 :: SquareTC a -> a
p2 (SquareDC x y) = y

-- to get a graph, get subset after squaring
data Graph a = GraphConstructor ( (SquareTC a) -> Bool)
-- contramap uses (g :: a -> b) to send (r :: (SquareTC b) -> Bool) to 
-- (wrapped) result of doing r after (g times g) 
instance MyContravariant Graph where
  myContramap g (GraphConstructor r) = GraphConstructor (\v -> (r (SquareDC (g (p1 v)) (g (p2 v)))))
-- for example, we make a graph, with vertices as integers, and directed edges representing divisibility
divisMap :: (SquareTC Int) -> Bool
divisMap (SquareDC x y) = (0 == (mod x y))
myBigGraph = GraphConstructor divisMap
-- use contramap to change this into graph showing which words pairs have divisible lengths
newGraph = myContramap stringLength myBigGraph
extractGraphMap :: Graph a -> ((SquareTC a) -> Bool)
extractGraphMap (GraphConstructor r) = r
getOutGraph = extractGraphMap newGraph
testResult2 = getOutGraph (SquareDC "door" "it")
-- returns true, because the length of "it" divides the length of "door"

intersection :: (a -> Bool) -> (a -> Bool) -> (a -> Bool) 
intersection f g = (\x -> ((f x) && (g x)))
--containedInQ :: (a -> Bool) -> (a -> Bool) -> Bool
--containedInQ f g = (intersection f g == f)
union :: (a -> Bool) -> (a -> Bool) -> (a -> Bool) 
union f g = (\x -> ((f x) || (g x)))

evenSelector :: Int -> Bool
evenSelector x = ((mod x 2) == 0)

oddSelector :: Int -> Bool
oddSelector x = ((mod x 2) == 1)

unionSelector = union evenSelector oddSelector

restResult3 = map unionSelector [1..7]

ff x = 2 * x
gg x = x + x

hh x = x + 7
