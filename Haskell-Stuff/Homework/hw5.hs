-- Kelvin Kemper
-- HW5
-- Due April 17th, 2023

-- Exercise 1
-- myTakeWhile (/= ' ') "This is practice." => "This"
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x:xs)
  | pred x = x : myTakeWhile pred xs
  | otherwise = []

-- Exercise 2
-- mySpan (/= ' ') "This is practice." => ("This"," is practice.")
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([], [])
mySpan p (x:xs) 
  | p x = let (ys, zs) = mySpan p xs in (x:ys, zs)
  | otherwise = ([], x:xs)


-- Exercise 3
-- combinations3 "ABCDE" => ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"] 
combinations3 :: (Ord a) => [a] -> [[a]]
combinations3 xs = [[x,y,z] | x <- xs, y <- xs, z <- xs, x < y, y < z]

-- Exercise 4
-- runLengthEncode [4, 2, 2, 1, 1, 1, 1, 4, 4, 4, 4] => [(4,1),(2,2),(1,4),(4,4)]
-- runLengthEncode "foo" => [('f',1),('o',2)]
runLengthEncode :: Eq a => [a] -> [(a, Int)]
--runLengthEncode xs = [(head ys, length ys) | ys <- group xs]
runLengthEncode xs = reverse $ foldl f [] xs
  where f [] x = [(x, 1)]
        f ((y,n):ys) x | x == y    = (y,n+1):ys
                       | otherwise = (x,1):(y,n):ys

-- Exercise 5
-- runLengthDecode [(4, 1), (2, 2), (1, 4), (4, 4)] => [4,2,2,1,1,1,1,4,4,4,4]
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode [] = []
runLengthDecode ((x, n):xs) = replicate n x ++ runLengthDecode xs

-- Exercise 6
-- splitText (/= ' ') "This is practice." => ["This","is","practice."]
splitText :: (Char -> Bool) -> [Char] -> [[Char]]
splitText p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitText p s''
                        where (w, s'') = break p s'

-- Exercise 7
-- encipher ['A' .. 'Z'] ['a' .. 'z'] "THIS" => "this"
index' x xs = indexHelper x xs 0
  where indexHelper _ [] _ = Nothing
        indexHelper y (z:zs) i
          | y == z    = Just i
          | otherwise = indexHelper y zs (i+1)

encipher :: Eq a => [a] -> [a] -> [a] -> [a]
encipher from to message = [ to !! i | x <- message, let i = index from x, i >= 0 ]
    where index xs x = case index' x xs of
                        Nothing -> -1
                        Just i -> i

-- Exercise 8
-- goldbach 6 => [(3,3)]
primes' n = [z | z <- [1..n], length (factors z) == 2]
    where factors n = [(x,y) | x <- [1..n], y <- [1..n], x*y == n]
    
goldbach :: Int -> [(Int, Int)]
goldbach n = [(x,y) | x <- primes' n, y <- primes' n, x + y ==n]
  
-- Exercise 9
-- increasing "ABBD" => False
-- increasing [100, 99 .. 1] => False
increasing :: Ord a => [a] -> Bool
increasing xs = and [x <= y | (x,y) <- zip xs (tail xs)]

-- Exercise 10
-- select even [1 .. 26] ['a' .. 'z'] => "bdfhjlnprtvxz"
-- select (<= 'g') ['a' .. 'z'] [1 .. 26]  => [1,2,3,4,5,6,7]
select :: (t -> Bool) -> [t] -> [a] -> [a]
select pred xs ys = [ys !! i | (i, x) <- zip [0..] xs, pred x]

-- Exercise 11
-- combinations 3 "ABCDE" => ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]
combinations :: Ord a => Int -> [a] -> [[a]]
combinations _ _ = undefined
  
-- Exercise 12
-- real (ComplexInteger 1 2) => 1
-- imaginary (ComplexInteger 2 3) => 3
-- (ComplexInteger 1 2) == (ComplexInteger 3 4) => False
-- (ComplexInteger 1 2) => 1+2i
-- (ComplexInteger 1 2) * (ComplexInteger 3 4) => -5+10i

data ComplexInteger = ComplexInteger Integer Integer

instance Eq ComplexInteger where
  (==) :: ComplexInteger -> ComplexInteger -> Bool
  (ComplexInteger a b) == (ComplexInteger c d) = a == c && b == d

instance Show ComplexInteger where
  show (ComplexInteger r i)
    | i >= 0 = show r ++ "+" ++ show i ++ "i"
    | otherwise = show r ++ show i ++ "i"

instance Num ComplexInteger where
  (ComplexInteger r1 i1) + (ComplexInteger r2 i2) = ComplexInteger (r1+r2) (i1+i2)
  (ComplexInteger r1 i1) * (ComplexInteger r2 i2) = ComplexInteger (r1*r2 - i1*i2) (r1*i2 + r2*i1)
  abs (ComplexInteger r1 i1) = ComplexInteger (abs r1) (abs i1)
  signum (ComplexInteger r1 i1) = ComplexInteger (signum r1) 0
  fromInteger n = ComplexInteger n 0
  negate (ComplexInteger r i) = ComplexInteger (negate r) (negate i)

real :: ComplexInteger -> Integer
real (ComplexInteger r _) = r

imaginary :: ComplexInteger -> Integer
imaginary (ComplexInteger _ i) = i