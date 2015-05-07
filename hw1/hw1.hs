-- Homework 1 of DSL 2014 Fall
-- Zhenfei Nie


-- ### Question 1
-- a)
--    1) show :: Show a => a -> String
--    The function show is to represent anything with type-class 'Show' to a String.
--    
--    2) map :: (a -> b) -> [a] -> [b]  
--    The function takes a function, which takes one thing and return another thing, 
--    and a list and applies that function to every element in the list to produce a new list. 
--
--    3) zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--    It takes a function, which takes two things and return another thing, and two lists and joins the two lists 
--    by applying the function between element-pairs in the two lists.

-- b) The funtion is :
--    renderHtmlWithLanguage :: HTML html => String -> html -> String
--    Outputs indented HTML, with indentation inside elements. (Description is taken from documentation.)


-- ### Question 2
-- a) 1 pi = 3.14
--    2
--    3 circumference r = let pi = 3.14159
--    4         in 2 * pi * r
--    5
--    6 area r = pi * r * r
--    
--    ANS: The use of pi in line 4 is bound at line 3.

-- b) 
--    1 x = 3
--    2
--    3 foo x = case x of
--    4 0 -> 0
--    5 x -> (let
--    6     x = y + 1
--    7     in y) * foo (x - 1) where
--    8     y = x + 1
--    9
--    10 y = x + foo x
--
--    ANS: 
--        The use of x in line 3 is bound at LINE 10 (the `foo x`).
--        The use of y in line 6 is bound at LINE 8.
--        The use of y in line 7 is bound at LINE 6.
--        The use of x in line 7 is bound at LINE 5.
--        The use of x in line 8 is bound at LINE 5.
--        The use of x in line 10 is bound at LINE 1.

-- ### Question 3
-- a) A three element tuple.
--      (3, 4, "hello")

-- b) A list of all integers between 1 and 10 (inclusive).
--      [1..10]

-- c) A list of tuples, where each tuple has a string as its first element 
--    and a character as its second element.
--      [("hello", 'a'), ("world", 'b')]

-- d) A function that takes two integers as parameters and returns a boolean.
--      The answer is to implement a funtion which compares two integers and
--      returns true if they equal.
--      (====) :: Int -> Int -> Bool
--      (====) a b = a == b
(====) :: Int -> Int -> Bool
(====) a b = a == b

-- e) Unit
--      ()

-- f) Maybe Integer
--      Just (6::Int)

-- g) [[[Char]]].
--      [ [ "hello" ], [ "world" ] ]

-- ### Question 4
-- a)
multiply :: Int -> Int -> Int
multiply 1 b = b
multiply a 1 = a
multiply 0 b = 0
multiply a 0 = 0
multiply a b
    | a > 1 = b + multiply (a-1) b
    | b > 1 = a + multiply a (b-1)
    | otherwise = multiply (-a) (-b)
 

-- b)
absolute :: Int -> Int
absolute x 
    | x < 0 = -x
    | otherwise = x

-- c)
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True


---- d) 
largest :: [Int] -> Int
largest [] = error "Cannot accept an empty list!"
largest [x] = x
largest (x:xs) = max x ( largest xs ) 

---- e)
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead [x] = Just x
maybeHead (x:xs) = Just x

-- -- --
-- END
-- -- --
main :: IO ()
main = putStrLn ( "The functions multiply, absolute, nand, largest" 
                ++ "and maybeHead have already been done.\nPlease check the code." )







