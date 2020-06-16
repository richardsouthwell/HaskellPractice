box :: (Int, Int) -> [Int]
box (x, y) = [x, y]

updateWithRewrite :: (Int -> (Int, Int)) -> [Int] -> [Int]
updateWithRewrite rule list = concat (map ( box . rule) list)

-- Thue–Morse sequence 

myMap :: Int -> (Int, Int)
myMap 0 = (0, 1)
myMap 1 = (1, 0)
myMap x = (2, 2)

--result = updateWithRewrite myMap [0]

myUp :: [Int] -> [Int]
myUp list = updateWithRewrite myMap list


nest :: (a -> a) -> a -> Int -> a
nest f x 0 = x
nest f x n = f (nest f x (n - 1))

result = nest myUp [0] 8
