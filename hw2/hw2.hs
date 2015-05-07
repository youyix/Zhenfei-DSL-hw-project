-- DSL hw 2 Zhenfei Nie

-- 1
-- a)
import Data.List
repeatString :: String -> Int -> String 
repeatString _ 0 = []
repeatString x n = intercalate "" [x, repeatString x (n-1)]
--repeatString x n = x ++ repeatString x (n-1) 

-- b)
pow :: Float -> Int -> Float
pow _ 0 = 1
pow 0 _ = 0
pow x n = x * pow x (n-1)

-- c) 
pow' :: Float -> Int -> Float 
pow' _ 0 = 1
pow' 0 _ = 0
pow' x 1 = x
pow' x n = if even n 
            then (pow' x m) * (pow' x m)
            else x * pow' x (n-1)
            where m = n `div` 2

-- d) 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- e)
fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = ( xs !! (n-1) ) + ( xs !! (n-2)) 
        where xs = 0 : 1 : map fib' [2..]

-- f)
-- 


-- #2 
-- a)
data Nat = Zero | Succ Nat deriving Show

-- b)
toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ ( toNat (n-1) )

-- c)
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x

-- d)
plus :: Nat -> Nat -> Nat
plus Zero y = y
plus (Succ x) y = Succ (plus x y)

-- e)
minus :: Nat -> Nat -> Nat
minus Zero _ = Zero
minus (Succ x) Zero = Succ x
minus (Succ x) (Succ y) = minus x y


-- #3
-- a)

keepIf :: (Int -> Bool) -> [Int] -> [Int]  
keepIf _ [] = []  
keepIf f (x:xs)   
    | f x       = x : keepIf f xs  
    | otherwise = keepIf f xs 


-- b)
removeZeros :: [Int] -> [Int]
removeZeros [] = []
removeZeros xs = keepIf (/= 0) xs

-- c)
applyToAll :: (Int -> Int) -> [Int] -> [Int]
applyToAll _ [] = []
applyToAll f (x:xs) = f x : applyToAll f xs

-- d)
reduce :: ( b -> a -> b ) -> b -> [a] -> b
reduce f b [] = b
reduce f b (a:xs) = reduce f (f b a) xs

-- e)
keepIf' :: (Int -> Bool) -> [Int] -> [Int]
keepIf' _ [] = []
keepIf' f xs = reduce (\b y -> b ++ if (f y) then y:[] else [] ) [] xs   


-- f)
applyToAll' :: (Int -> Int) -> [Int] -> [Int]
applyToAll' _ [] = []
applyToAll' f xs = reduce (\b y -> b ++ (f y):[] ) [] xs


    



















