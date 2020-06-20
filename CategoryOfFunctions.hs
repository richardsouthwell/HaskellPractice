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

----------------------------------------

functionsToMaps :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> (a1 -> b1) -> (a0 -> b0) -> (Map a1 b1, Map a0 b0)
functionsToMaps (sa1, ma, sa0) fL fV = (Data.Map.fromList (Prelude.map (\v -> (v, fL v)) (Data.Set.toList sa1)), Data.Map.fromList (Prelude.map (\v -> (v, fV v)) (Data.Set.toList sa0)))



-------------------------------------- products

myProduct :: Ord a => Ord b => Ord c => Ord d => Obj a b -> Obj c d -> Obj (a,c) (b,d)
myProduct (a,f,b) (c,g,d) = (aTimesc, Data.Map.fromList (Prelude.map (\v -> (v, ((f !) (fst v),(g !) (snd v)))) (Data.Set.toList aTimesc)), Data.Set.cartesianProduct b d) where aTimesc = Data.Set.cartesianProduct a c

getP1 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj (a1,b1) (a0,b0) -> (Map (a1,b1) a1, Map (a0,b0) a0)
getP1 (ds1, dm, ds0) = (Data.Map.fromList (Prelude.map (\v -> (v, fst v)) (Data.Set.toList ds1)), Data.Map.fromList (Prelude.map (\v -> (v, fst v)) (Data.Set.toList ds0)))

-- run getP1 on (myProduct (a,f,b) (c,g,d)) to get first projection 

getP2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj (a1,b1) (a0,b0) -> (Map (a1,b1) b1, Map (a0,b0) b0)
getP2 (ds1, dm, ds0) = (Data.Map.fromList (Prelude.map (\v -> (v, snd v)) (Data.Set.toList ds1)), Data.Map.fromList (Prelude.map (\v -> (v, snd v)) (Data.Set.toList ds0)))


-- p1L == p1V == fst 
-- p2L == p2V == snd

intermed :: Ord z1 => Ord z0 => Ord a1 => Ord a0 => Ord b1 => Ord b0 => (Map z1 a1, Map z0 a0) -> (Map z1 b1, Map z0 b0) -> (Map z1 (a1, b1), Map z0 (a0,b0))
intermed (fL, fV) (gL, gV) = (Data.Map.fromList (Prelude.map (\v -> ((fst v), ((fL !) (fst v), (gL !) (fst v))))  (Data.Map.toList fL)), Data.Map.fromList (Prelude.map (\v -> ((fst v), ((fV !) (fst v), (gV !) (fst v))))  (Data.Map.toList fV))) 

--myProduct (a,f,b) (c,g,d) = (((aTimesc, Data.Map.fromList lis, Data.Set.cartesianProduct b d) where lis = Prelude.map ( mymap) (Data.Set.toList aTimesc)) where mymap = (  \v -> (v, ((f !) v,(g !) v))) ) where aTimesc = Data.Set.cartesianProduct a c

xx :: Map Int (Map Int String)
xx = Data.Map.fromList [(1, Data.Map.fromList [(2,"dog"), (3,"cat")])]


-- each map from Obj(a1,a0) to Obj(b1,b0)corresponds to a member of (Map(a1,b1),Map(a0,b0)), and also to a point of b^a
--myExpObj :: Obj(a1,a0) -> Obj(b1,b0) -> Obj((Map(a1,b1),Map(a0,b0)),Map(a0,b0))

-- code to enumerate all such maps
-- add projection maps, intermediate arrows, object validity checker, and arrow validaty checker

-- can make is member setup, so we can properly talk about power sets


---------------------------------------------- exponentials --------------------

appendWays :: [(a,b)] -> a -> [b] -> [[(a,b)]]
-- list all the was to extend pv by appending (av,x) where x is in list bs
appendWays pv av bs = Prelude.map (\v -> pv ++ [(av,v)]) bs

-- appendWays [(1,"x"),(2,"y")] 3 ["x","y","z"]

-- [(*) 2 q | q <- [1..3]]

extend :: [[(a,b)]] -> a -> [b] -> [[(a,b)]]
-- loop at each list of pairs, replace it with the list of lists of pairs we can get by gluing (av,x):x in bs 
-- to the end, and then flatten the result
extend llp av bs = concat (Prelude.map (\f -> appendWays f av bs) llp)

-- extend (appendWays [(1,"x"),(2,"y")] 3 ["x","y","z"]) 4 ["x","y","z"]

-- next just fold over a's

setOfFnsIt :: [[(a,b)]] -> [a] -> [b] -> [[(a,b)]]

setOfFnsIt llp [] bs = llp
-- folds, extending over every a
setOfFnsIt llp (av:as) bs = setOfFnsIt (extend llp av bs) as bs

myStart = extend [[]] 1 ["x","y"]
myTest = setOfFnsIt myStart [2] ["x", "y"]

setOfFns :: [a] -> [b] -> [[(a,b)]]
setOfFns as bs = setOfFnsIt [[]] as bs
-- returns a list of all the functions from [a] to [b] (with everything represented by lists)
-- test the above vs `tuples' code + fold based code


-- setOfFns [1,2] ["x","y"]

independentVertices :: Ord b1 => Ord b0 => Obj b1 b0 -> [b0]
independentVertices (sa,mab,sb) = Data.Set.toList (Data.Set.difference sb (Data.Set.fromList (Prelude.map snd (Data.Map.toList mab))))

--independentVertices (Data.Set.fromList [1], Data.Map.fromList [(1,"x")], Data.Set.fromList ["x", "y"])

independentVertexMaps :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Map a1 b1 -> [[(a0,b0)]]
independentVertexMaps (sa1, ma, sa0) (sb1, mb, sb0) loopMap =  setOfFns (independentVertices (sa1,ma,sa0)) (Data.Set.toList sb0)

--loopMapChildren :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Map a1 b1 -> [([(a1,b1)],[(a0,b0)])]
--loopMapChildren (sa1, ma, sa0) (sb1, mb, sb0) loopMap = where im = setOfFns (independentVertices (sa1,ma,sa0)) (Data.Set.toList sb0)

-- Map \v -> (loopMap, inducedmap ++ v) im

loopMapChildren :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Map a1 b1 -> [(Map a1 b1, Map a0 b0)]
-- lists the arrows from (Obj a1 a0) to (Obj b1 b0) which map loops like loopMap does 
loopMapChildren (sa1, ma, sa0) (sb1, mb, sb0) loopMap = Prelude.map (\v -> (loopMap,Data.Map.union (Data.Map.fromList (Prelude.map (\v -> ((ma ! v), (mb ! (loopMap ! v)))) (Data.Set.toList sa1))) (Data.Map.fromList v))) im where im = setOfFns (independentVertices (sa1,ma,sa0)) (Data.Set.toList sb0)
--vertexMapInducedByLoopMap = Data.Map.fromList (Prelude.map (v -> ((ma ! v), (mb ! (loopMap ! v)))) (Data.Set.toList sa1))
 -- send loop to (loop's_vertex, loop's_image_vertex)
-- output type of loopMapChildren is [(Map a1 b1, Map a0 b0)]

allArrows :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> [(Map a1 b1, Map a0 b0)]
-- pair each loop with their child-maps joined to their independent vertex parts, result is a list of all a -> b
allArrows (sa1, ma, sa0) (sb1, mb, sb0) = concat (Prelude.map (\v -> loopMapChildren (sa1, ma, sa0) (sb1, mb, sb0) v) (Prelude.map Data.Map.fromList (setOfFns (Data.Set.toList sa1) (Data.Set.toList sb1))))

allVertexMaps :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> [Map a0 b0]
allVertexMaps (sa1, ma, sa0) (sb1, mb, sb0) = Prelude.map Data.Map.fromList (setOfFns (Data.Set.toList sa0) (Data.Set.toList sb0))

-- presumably map and set automatically order, so action works properley in the exponential object

exponentialObject :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Obj (Map a1 b1, Map a0 b0) (Map a0 b0)
-- loops~arrows, vertices~VertexMaps, action sends arrow to its vertex-component
exponentialObject (sa1, ma, sa0) (sb1, mb, sb0) = (Data.Set.fromList arl, Data.Map.fromList (Prelude.map (\v -> (v,snd v)) arl), Data.Set.fromList (allVertexMaps (sa1, ma, sa0) (sb1, mb, sb0))) where arl = allArrows (sa1, ma, sa0) (sb1, mb, sb0)

-- component of evaluation arrow on loops
evaluationArrowL :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => (a1, (Map a1 b1, Map a0 b0)) -> b1
-- sends (a-loop, arrow) to the result of doing the arrow on the loop
evaluationArrowL (a1, (m1, m0)) = (m1 !) a1

evaluationArrowLL :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => ((Map a1 b1, Map a0 b0), a1) -> b1
-- sends (a-loop, arrow) to the result of doing the arrow on the loop
evaluationArrowLL ((m1, m0), a1) = (m1 !) a1


-- component of evaluation arrow on vertices
evaluationArrowV :: Ord a0 => Ord b0 => (a0, Map a0 b0) -> b0
-- sends (a-vertex, vertex-map) to the result of doing the vertex-map on the vertex
evaluationArrowV (a0, m0) = (m0 !) a0

-- component of evaluation arrow on vertices
evaluationArrowVV :: Ord a0 => Ord b0 => (Map a0 b0, a0) -> b0
-- sends (a-vertex, vertex-map) to the result of doing the vertex-map on the vertex
evaluationArrowVV (m0, a0) = (m0 !) a0

evaluationArrow :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> (Map ((Map a1 b1, Map a0 b0), a1) b1, Map ((Map a0 b0), a0) b0)
evaluationArrow (sa1, ma, sa0) (sb1, mb, sb0) = functionsToMaps (myProduct (exponentialObject (sa1, ma, sa0) (sb1, mb, sb0)) (sa1, ma, sa0)) evaluationArrowLL evaluationArrowVV
--

--evaluationArrow :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> (Map (a1, (Map a1 b1, Map a0 b0)) b1, Map (a0, (Map a0 b0)) b0)
--evaluationArrow (sa1, ma, sa0) (sb1, mb, sb0) = functionsToMaps (myProduct (exponentialObject (sa1, ma, sa0) (sb1, mb, sb0)) (sa1, ma, sa0)) evaluationArrowL evaluationArrowV


--evaluationArrow :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> (Map (a1, (Map a1 b1, Map a0 b0)) b1, Map (a0, (Map a0 b0)) b0)
--evaluationArrow (sa1, ma, sa0) (sb1, mb, sb0) = (Data.Map.fromList (Prelude.map (\v -> (v, evaluationArrowL v)) (Data.Set.toList (t1 res))), Data.Map.fromList (Prelude.map (\v -> (v, evaluationArrowV v)) (Data.Set.toList (t3 res)))) where res = myProduct (exponentialObject (sa1, ma, sa0) (sb1, mb, sb0)) (sa1, ma, sa0) 


--getEvaluationArrow :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj (a1, (Map a1 b1, Map a0 b0)) (a0, (Map a0 b0)) -> (Map (a1, (Map a1 b1, Map a0 b0)) b1, Map (a0, (Map a0 b0)) b0)
--getEvaluationArrow xx ff yy =  (Prelude.map (\v -> (v, evaluationArrowL v)) (Data.Set.toList xx), Prelude.map (\v -> (v, evaluationArrowV v)) (Data.Set.toList yy))
-- do getEvaluationArrow (myProduct (exponentialObject (sa1, ma, sa0) (sb1, mb, sb0)) (sa1, ma, sa0))

--Map (a1, (Map a1 b1, Map a0 b0)) b1
--(\v -> (fst v, ) )
--getEvaluationArrow :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj (Map a1 b1, Map a0 b0) (Map a0 b0) -> (Map (a1, (Map a1 b1, Map a0 b0)) b1, Map (a0, (Map a0 b0)) b0)
--getEvaluationArrow (sa1, ma, sa0) (sm1, mm, sm0) = 
-- (a1, (m1, m0)) = (m1 !) a1


transposeL :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Ord c1 => Ord c0 => Obj a1 a0 -> Obj c1 c0 -> (Map (c1,a1) b1, Map (c0,a0) b0) -> c1 -> (Map a1 b1, Map a0 b0)
-- given arrow (fL, fV) from c * a to b, transposeL sends a loop c1 of c, to the (a -> b) arrow fL(c1,_) which sends loop a1 of a to fL(c1,a1), and which sends vertex a0 of a to fV(vertex c1,a0)  
transposeL (sa1, ma, sa0) (sc1, mc, sc0) (fL, fV) c1 = (Data.Map.fromList (Prelude.map (\v -> (v, (fL !) (c1, v))) (Data.Set.toList sa1)), Data.Map.fromList (Prelude.map (\v -> (v, (fV !) (((mc !) c1), v))) (Data.Set.toList sa0)) )

transposeV :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Ord c1 => Ord c0 => Obj a1 a0 -> Obj c1 c0 -> (Map (c1,a1) b1, Map (c0,a0) b0) -> c0 -> Map a0 b0
-- given arrow (fL, fV) from c * a to b, transposeV sends a vertex c0 of c, to the vertex map fV(c0,_) which sends vertex a0 of a to fV(c0,a0)  
transposeV (sa1, ma, sa0) (sc1, mc, sc0) (fL, fV) c0 = Data.Map.fromList (Prelude.map (\v -> (v,(fV !) (c0, v))) (Data.Set.toList sa0)) 

getTranspose :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Ord c1 => Ord c0 => Obj a1 a0 -> Obj c1 c0 -> (Map (c1,a1) b1, Map (c0,a0) b0) -> (Map c1 (Map a1 b1, Map a0 b0),Map c0 (Map a0 b0))
getTranspose (sa1, ma, sa0) (sc1, mc, sc0) (fL, fV) = (Data.Map.fromList (Prelude.map (\v -> (v, transposeL (sa1, ma, sa0) (sc1, mc, sc0) (fL, fV) v)) (Data.Set.toList sc1)), Data.Map.fromList (Prelude.map (\v -> (v, transposeV (sa1, ma, sa0) (sc1, mc, sc0) (fL, fV) v)) (Data.Set.toList sc0)))

-- make code to change a function specification to a map and apply it to write eval and transpose as maps

--functionsToMaps :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> (a1 -> b1) -> (a0 -> b0) -> (Map a1 b1, Map a0 b0)




-- make internalization of composition

-- make arrow composition operator

-- to do: projections, intermediary, subobject classifier, arrow test, object test
-- MBL
-- check that enumerating arrows into omega gives correct results
-- tests
-- limits + colimits
-- visualization    



kk= exponentialObject ex1 ex1 

-- :t t1 (myProduct kk ex1)

-- open up an example, and get eval map properley

e1 = (Data.Set.fromList ['a', 'b'], Data.Map.fromList [('a',1),('b',2)], Data.Set.fromList [1, 2])
e2 = (Data.Set.fromList [True, False], Data.Map.fromList [(True, 0.1),(False, 0.2)], Data.Set.fromList [ 0.1, 0.2])

myExp = exponentialObject e1 e2

myObj = myProduct myExp e1

myObjLoops = Data.Set.toList (t1 myObj)

--getLoops :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> [((Map a1 b1, Map a0 b0), a0)]
--getLoops (e1A, e1B, e1C) (e2A, e2B, e2C) = Data.Set.toList (t1 (myProduct (exponentialObject (e1A, e1B, e1C) (e2A, e2B, e2C)) (e1A, e1B, e1C)))

--(e1A, e1B, e1C) (e2A, e2B, e2C)

exponentialObject2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Obj (Map a1 b1, Map a0 b0) (Map a0 b0)
exponentialObject2 (sa1, ma, sa0) (sb1, mb, sb0) = exponentialObject (sa1, ma, sa0) (sb1, mb, sb0)

myObj2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Obj ((Map a1 b1, Map a0 b0), a1) (Map a0 b0, a0)
myObj2 (sa1, ma, sa0) (sb1, mb, sb0) = myProduct theExp (sa1, ma, sa0) where theExp = (exponentialObject2 (sa1, ma, sa0) (sb1, mb, sb0))

myVertices2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> [(Map a0 b0, a0)]
myVertices2 (sa1, ma, sa0) (sb1, mb, sb0) = Data.Set.toList (t3 dataa) where dataa = myObj2 (sa1, ma, sa0) (sb1, mb, sb0)

--evaluationArrowV :: Ord a0 => Ord b0 => (a0, Map a0 b0) -> b0
-----------------------
--myVertexMap2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Map (Map a0 b0, a0) b0
--myVertexMap2 (sa1, ma, sa0) (sb1, mb, sb0) = Data.Map.fromList (Prelude.map )
--Obj ((Map a1 b1, Map a0 b0), a1) (Map a0 b0, a0)
 

--myObj3 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> (Set ((Map a1 b1, Map a0 b0), a1), Map ((Map a1 b1, Map a0 b0), a1) (Map a0 b0, a0), Set (Map a0 b0, a0))
--myObj3 (sa1, ma, sa0) (sb1, mb, sb0) = myProduct theExp (sa1, ma, sa0) where theExp = (exponentialObject2 (sa1, ma, sa0) (sb1, mb, sb0))

--myVertices2b :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Set (Map a0 b0, a0)
--myVertices2b (sa1, ma, sa0) (sb1, mb, sb0) = u3 where (u1, u2, u3) = myObj3 (sa1, ma, sa0) (sb1, mb, sb0)


--myVertices2b :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> Set (Map a0 b0, a0)
--myVertices2b (sa1, ma, sa0) (sb1, mb, sb0) = (t1 dataa) where dataa = myObj3 (sa1, ma, sa0) (sb1, mb, sb0)


--myVertices2 :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> Obj b1 b0 -> [(Map a0 b0, a0)]
--myVertices2 (sa1, ma, sa0) (sb1, mb, sb0) = Data.Set.toList (t1 dataa) where dataa = myObj2 (sa1, ma, sa0) (sb1, mb, sb0)


-----------------------------------
-- comp works as "after", for arrow composition
comp :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Ord c1 => Ord c0 => (Map b1 c1, Map b0 c0) -> (Map a1 b1, Map a0 b0) -> (Map a1 c1, Map a0 c0)
-- takes in two arrows and returns their composition
comp (g1, g0) (f1, f0) = (Data.Map.map (g1 !) f1, Data.Map.map (g0 !) f0)



--map3 = Data.Map.fromList [("a",2), ("b",3)]
--map4 = Data.Map.fromList [(2,3), (3,7)]
--map5 = Data.Map.map (map4 !) map3

inclusion :: Ord a1 => Ord a0 => Obj a1 a0 -> (Map a1 a1, Map a0 a0)
-- arrow that sends all vertices and loops to self
-- whether this is an identity map or some other inclusion depends on the target object, which is unspecified here
inclusion (sa1, ma, sa0) = (Data.Map.fromList (Prelude.map (\v -> (v,v)) (Data.Set.toList sa1)), Data.Map.fromList (Prelude.map (\v -> (v,v)) (Data.Set.toList sa0)))
-- inclusion could describe an identity arrow, or an inclusion map depending on the context


terminalObject :: Obj String String
terminalObject = (Data.Set.fromList ["t"], Data.Map.fromList [("t","tv")], Data.Set.fromList ["tv"])

arrowToTerminal :: Ord a1 => Ord a0 => Obj a1 a0 -> (Map a1 String, Map a0 String)
-- returns the arrow from an objec to the terminal object
arrowToTerminal (sa1, ma, sa0) = (Data.Map.fromList (Prelude.map (\v -> (v,"t")) (Data.Set.toList sa1)), Data.Map.fromList (Prelude.map (\v -> (v,"tv")) (Data.Set.toList sa0)))


singleVertex :: Obj String String
singleVertex = (Data.Set.fromList [], Data.Map.fromList [], Data.Set.fromList ["tv"])

-- action arrow is s_{->} goes from single vertex to terminal object
actionArrow :: (Map String String, Map String String)
actionArrow = inclusion singleVertex

omega :: Obj String String
-- subobject classifier
omega = (Data.Set.fromList ["t", "m", "f"], Data.Map.fromList [("t","tv"), ("m","tv"), ("f","fv")], Data.Set.fromList ["tv", "fv"])

trueArrow :: (Map String String, Map String String)
-- true arrow as inclusion
trueArrow = inclusion terminalObject

subobjectClassifiedBy :: Ord a1 => Ord a0 => Obj a1 a0 -> (Map a1 String, Map a0 String) -> Obj a1 a0
-- input: object + arrow from object to chi
-- outputs a substructure of Obj a1 a0, which we imagine is equipped with an inclusion map
-- here (chi1, chi0) is an arrow from Obj a1 a0  to omega
subobjectClassifiedBy (sa1, ma, sa0) (chi1, chi0) = (Data.Set.fromList remainingLoops, Data.Map.fromList (Prelude.map (\v -> (v, (ma !) v)) remainingLoops), Data.Set.fromList (Prelude.map fst (Prelude.filter (\v -> ((snd v) == "tv")) (Data.Map.toList chi0))) ) where remainingLoops = Prelude.map fst (Prelude.filter (\v -> ((snd v) == "t")) (Data.Map.toList chi1))

-- assuming monic is inclusion map
-- (we can add monic test, and convert any monic to its equivalent inclusion map
-- just by using `convertToInclusion')
classifyingArrowOfInclusion :: Ord a1 => Ord a0 => Obj a1 a0 -> Obj a1 a0 -> (Map a1 String, Map a0 String)
classifyingArrowOfInclusion (sa1, ma, sa0) (usa1, uma, usa0) = (Data.Map.fromList (Prelude.map (\v -> (v, if (Data.Set.member v usa1) then "t" else (if (Data.Set.member ((ma !) v) usa0) then "m" else "f" ))) (Data.Set.toList sa1)), Data.Map.fromList (Prelude.map (\v -> (v, if (Data.Set.member v usa0) then "tv" else "fv")) (Data.Set.toList sa0)))
-- input: object + substructure, output: arrow to chi








--member :: Ord a => a -> Set a -> Bool


-- equivalent inclusion map to monic
-- here we just return the substructure, but when its equipped with its inclusion, job is done
convertToInclusion :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => (Map a1 b1, Map a0 b0) -> Obj b1 b0  -> Obj b1 b0 
convertToInclusion (i1, i0) (sb1, mb, sb0) = (Data.Set.fromList remainingLoops, Data.Map.fromList (Prelude.map (\v -> (v, (mb !) v)) remainingLoops), Data.Set.fromList (Prelude.map snd (Data.Map.toList i0))) where remainingLoops = Prelude.map snd (Data.Map.toList i1)
-- input: monic + target object of monic, output: substructure of target object

--convertToInclusion :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => Obj a1 a0 -> (Map a1 b1, Map a0 b0) -> Obj a1 a0 
--convertToInclusion (sa1, ma, sa0) (i1, i0) = (Data.Set.fromList remainingLoops, Data.Map.fromList (Prelude.map (\v -> (v, (ma !) v)) remainingLoops), Data.Set.fromList (Prelude.map snd (Data.Map.toList i0))) where remainingLoops = Prelude.map snd (Data.Map.toList i1)

classifyingArrowOf :: Ord a1 => Ord a0 => Ord b1 => Ord b0 => (Map a1 b1, Map a0 b0) -> Obj b1 b0 -> (Map b1 String, Map b0 String)
classifyingArrowOf (i1, i0) (sb1, mb, sb0) = classifyingArrowOfInclusion (sb1, mb, sb0) (convertToInclusion (i1, i0) (sb1, mb, sb0))
-- input: monic + target object of monic, output: classifying arrow of monic


-- initialObjects
