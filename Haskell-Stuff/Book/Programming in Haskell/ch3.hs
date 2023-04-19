--Types and Classes

--Basics: Bool type contains False and True..
-- Bool -> Bool contains all functions that map arguements from Bool to results from Bool

-- Using the notations v :: T means that v is a value in the type T
-- and that v has type T.
-- EXAMPLES:  False :: Bool , True :: Bool, not :: Bool -> Bool

-- All expressions must have a type which is calculated before evaluation,
-- and this is called type inference.
-- the rules state if f is a function that maps arguments of type A to results of type B,
    -- and e is an expression of type A, then the application f e has type B:
    -- f :: A -> B     e :: A
    --       f e :: B


-- LIST TYPES --
-- A list is a sequence of elements of the same type
-- [] is an empty list, [False], ['a'], and [[]] are singleton lists

-- TUPLE TYPES --
-- A tuple is a finite sequence fo componenets of possibly different types enclosed in 
    --round parenthesis ()

-- FUNCTION TYPES --
-- a function is mapping from arguments of one type to results of another type
-- Write T1 -> T2 for the types of all functs that map arguments of type T1 to result of type T2
-- example: not :: Bool -> Bool    || even :: Int -> Bool

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- CURRIED FUNCTIONS --
add' :: Int -> (Int -> Int)
add' x y = x + y

addone :: Num a => a -> a
addone x = x + 1

--Exercises--
-- 1. [Char], (Char, Char, Char), [(Bool,Char)], ([Bool],[Char]), [[a]->[a]]

second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a->b->(a,b)
pair x y = (x,y)


double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- for fun

futile pred x = 