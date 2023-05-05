concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- test if a number is prime with list comprehensions
-- need factors first

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
    where
        prime n = factors n == [1,n]

-- how to pair every element in a list with each other
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- sorted?
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- positions of i in a list of xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- wow
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

