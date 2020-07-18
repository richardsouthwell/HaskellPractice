data MyExpr = JJust Int | Add MyExpr MyExpr
evalE :: MyExpr -> Int
evalE (JJust a) = a
evalE (Add a b) = (+) (evalE a) (evalE b)
myExample = evalE (Add (JJust 1) (Add (JJust 4) (JJust 2)))

-- Informatics 1, Functional Programming Lecture 10
-- Expression Trees as Algebraic Data Types
-- Don Sannella
