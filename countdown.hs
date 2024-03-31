
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

-- evaluate the value of an expression
eval :: Expr -> [Int]
eval (Val n) = [n | n >0]
eval (App o l r) = [apply o x y | x <- eval l , y <- eval r , valid o x y]


-- Combinatorial functions 

-- first functions is the power set of an expression 
powerset :: [a]->[[a]]
powerset []=[[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


-- interleave an element through a function 

-- example [1,2,3] 7 = [[1,2,3,7] [1,2,7,3] [1,7,2,3] , [7,1,2,3]]

interleave:: a -> [a] ->[[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys): map (y:) (interleave x ys)


-- all permutations

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = concat (map (interleave x) (permutations' xs))

-- choices , all possible choices of commbinations from the list 
choices :: [a]->[[a]]
choices = concatMap permutations' . powerset

-- formalisating the problem 

solution :: Expr -> [Int] ->Int -> Bool 
-- elem checks if the values of e are infact a combination of values in the set ns
solution e ns n = elem (values e) (choices ns) && eval e == [n]


-- Brute force solution 

split :: [a]-> [([a],[a])]
split [] = []
split [_] = []
split (x:ys) = ([x],ys) : [(x:l,r)| (l,r)<- split ys]


ops :: [Op]
ops = [Add, Mul,Div,Sub]

combine :: Expr -> Expr -> [Expr]
combine x y = [App op x y | op <- ops]

-- all possible expressions of a list 
exprs ns = [e | (ls,rs) <- split ns , l <- exprs ls , r <- exprs rs, e <- combine l r]
-- final solution to the count down problem 
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns , e <- exprs ns' , eval e ==[n]]


-- optimised solutions 
-- first is to consider evaluating and generating at the same time 



type Result =(Expr,Int)
combine' :: Result -> Result->[Result]

combine' (l,x) (r,y) = [(App op l r, apply op x y)| op <- ops, valid op x y]



results :: [Int]-> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [result | (ls, rs) <- split ns , l <- results ls, r <- results rs, result <- combine' l r ]

solutions' ns n = [e | ns' <- choices ns , (e,m)<- results ns', m == n]




