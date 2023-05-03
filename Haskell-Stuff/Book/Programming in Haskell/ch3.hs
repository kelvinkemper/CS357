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

-- Curried Functions --
-- remember functions are free to return functions as results
-- different add from above

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

addone :: Num a => a -> a
addone x = x + 1

-- Polymorphic Types --
-- a type that contains one or more type variables is cally polymorphic
-- example length :: [a] -> Int means that any any type a returns an Int

-- Overloaded types --
-- These have class constraints written in the form of C a, where C is
-- the name of a class and a is a type variable. 
-- (+) :: Num a => a -> a -> a
-- We see that it has a type a that is an instance of the class Num
-- A type that contains one or more class constraints is called overloaded

-- Basic classes
-- a class is a collection of types that support certain overloaded ops call methods
-- Eq (equality types) ends as a bool so, Eq a => a -> a-> Bool
-- Ord (ordered types) instance of Eq but using orders (like (<) or (>) or (min))
-- Show (showable types) class contains types whose values can be toString
        -- using show :: a -> String
        -- example: show False ==> "False"
-- Read (readable types) the sister to Show
        -- using read :: String -> a
        -- example: read "'a'" :: Char ==> 'a', read "123" :: Int ==> 123
-- Num (numeric types) class contains types whose values are numeric
    -- can be processed by (+), (-), (*), negate, abs, and signum
-- Integral (integral types) this class contains types that are instance of the numeric class
-- class Num in addition whose values are integers and support the methods of integer and integer remainder
    -- uses 'div' and 'mod'


--Exercises--
-- 1. [Char], (Char, Char, Char), [(Bool,Char)], ([Bool],[Char]), [[a]->[a]]
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add'' :: Int -> Int -> Int -> Int
add'' x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f y = f y 

second' :: [a] -> a
second' xs = head (tail xs)

swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

pair' :: a -> b -> (a,b)
pair' x y = (x,y)

double' :: Num a => a -> a
double' x = x * 2

palindrome' :: Eq a => [a] -> Bool
palindrome' xs = reverse xs == xs

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

second :: [a] -> a
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

-- futile pred x = 