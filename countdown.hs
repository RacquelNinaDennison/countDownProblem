-- implementation of count down problem

-- define the operations that can be performed
data Op = Add | Mul | Div | Sub 

instance Show Op where
    show Add = "+"
    show Sub ="-"
    show Mul = "*"
    show Div = "/"

-- apply the operation to a set of intergers 
apply :: Op -> Int -> Int -> Int 
apply Add x y = x+y
apply Mul x y = x *y 
apply Sub x y = x -y 
apply Div x y = x `div` y 

-- check if the operation is valid to the problem description 


valid :: Op -> Int -> Int -> Bool 
valid Add x y = True
valid Mul x y  = True
valid Div x y =  x`mod`y ==0 
valid Sub x y = x > y 


-- optimised version 

valid' :: Op -> Int -> Int -> Bool 
valid' Add x y = x <= y
valid' Mul x y  = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x`mod`y ==0 
valid' Sub x y = x > y 


-- now we will create the expression 

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n) = show n 
    show (App o l r) = showExpression l ++ show o ++ showExpression r 
                        where 
                            showExpression (Val n) = show n 
                            showExpression e = "("++ show e ++ ")"

-- get all the integer values in an expression 
-- takes in an expression and returns the intergers 
values :: Expr -> [Int]
values (Val n) = [n]
values (App op l r) = values l ++ values r 

