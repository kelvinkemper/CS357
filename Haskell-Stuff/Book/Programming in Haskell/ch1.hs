double x = x + x

-- haskell type system detects incompatibility errors, called
-- type inference

-- list comprehension notation that constructs new lists by selecting
-- and filtering elements from one or more existing lists
-- (can be used without the need for explicit recursion)

-- higher-order functions means that functions can freely take functions as 
-- arguments and produce functions as results

-- lazy evaluation: based upon the idea that no computation should be performed 
-- until its result is actually required

sum' :: Num a => [a] -> a
sum' []     = 0 
sum' (n:ns) = n + sum' ns 
--first line states that sum of empty list is zero
--second line states that sum of any non-empty list comprising a first number n
-- and a remaining list of numbers ns is given by adding n and the sum of ns
-- ex: sum [1,2,3]
-- = 1 + sum[2,3]
-- = 1 + (2 + sum[3])
-- = 1 + (2 + (3 + sum []))
-- = 1 + (2 + (3 + 0))
-- = 6

-- Num a => [a] -> a
-- for any type a of numbers,
-- sum is a function that maps a list of such numbers
-- to a single such number

qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b > x]

reverseqsort [] = []
reverseqsort (x:xs) = reverseqsort larger ++ [x] ++ reverseqsort smaller
                      where
                            smaller = [a | a <- xs, a <= x]
                            larger  = [b | b <- xs, b > x]
