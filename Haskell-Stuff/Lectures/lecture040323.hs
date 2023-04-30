import Data.List (permutations)
--outerProduct f xs ys = map (\x -> map (\y -> f x y) ys) xs

select _ [] [] = []
select pred (x:xs) (y:ys) = if pred x then y : rest else rest
    where rest = select pred xs ys
--(lambda (x) (= x 0))
--(== 0)
primes n = map (filter (== 0)) (outerProduct rem [1..n] [1..n])

-- (lambda (xs) (cons x xs))
powerset [] = [[]]
powerset (x:xs) = subset ++ (map (x :) subset)
    where subset = powerset xs

--(x :) == left section of x
-- xs is a list
-- xss is a list of lists
-- xsss is a list of lists of lists
permutations' [] = [[]]
permutations' xs = concat $ map (\x -> (map (x :) (permutations' (filter (/= x) xs)))) xs

outerProduct f xs ys = [[f x y | x <- xs] | y <- ys]
--outerProduct (,) [1..5] ['a'..'f']
--outerProduct (*) [1..5] [1..5]

--- LOOK UP CARTESIAN PRODUCT --- 
select' :: (a -> Bool) -> [a] -> [b] -> [b]
select' pred xs ys = [y | (x,y) <- zip xs ys, pred x]

perfect n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a*a + b*b == c*c]

primes' n = select (== 2) (map length $ map (filter (== 0)) (outerProduct rem [1..n] [1..n])) [1..n]

factors n = [x | x <- [1..n], rem n x == 0]

factors' n = [x | x <- [1..n], y <- [1..n], x*y == n]

primes'' n = [z | z <- [1..n], length (factors z) == 2]

permutations'' [] = [[]]
permutations'' xs = [ x : perms | x <- xs, perms <- permutations''
 (filter (/= x) xs)]