-- syntax
-- create a list by using
-- myList = [1,2,3,4]
-- putting a list together (iterates from left to right), use ++
-- ex: "hello" ++ " " ++ "world"

-- cons a list use, :
-- : can combine mismatched elements while ++ must be 2 lists similar to append in scheme

-- the list [1,2,3] is actually 1:2:3:[] where [] is an empty list

-- use !! to return that element index a list
-- ex: "holasenor" !! 3 returns "a"

-- head is car
-- tail is cdr
-- last returns last element
-- init returns all but the last
-- null is null?
-- take is like substring by extract however many from beginning of list
    --ex: take 3 [5,4,3,2,1] ==> [5,4,3]
-- drop similar to take except returns the remaining list

-- elem is similar to member?
    -- ex: 4 'elem' [3,4,5,6] ==> True


-----RANGE IN A LIST-------
--want all numbers 1 through 20?
a = [1..20]
--how about all evens from 2 to 20?
evens = [2,4..20]
--or every third number?
thirdNumbers = [2,5..20]

-- if we want to repeat a list for a certain amt of elements or go to a certain value we can use:
    -- take 12 (cycle "LOL ") ==> "LOL LOL LOL "
    -- or
    -- take 10 (cycle [1,2,3]) ==> [1,2,3,1,2,3,1,2,3,1]
--EVEN EASIER use replicate function
-- replicate 3 10 ==> [10,10,10]


