-- Kelvin Kemper
-- Homework #6
-- Due May 3rd

-- Exercise 1
bits2num :: String -> Integer
bits2num = bits2numHelper 0

bits2numHelper :: Integer -> String -> Integer
bits2numHelper acc [] = acc
bits2numHelper acc (x:xs) = bits2numHelper (acc * 2 + fromIntegral (read [x])) xs

-- Exercise 2
num2bits :: Int -> String
num2bits 0 = "0"
num2bits 1 = "1"
num2bits n = num2bits (div n 2) ++ show (mod n 2)

-- Exercise 3
variance :: Fractional a => [a] -> a
variance xs = sum squaredDifferences / fromIntegral (length xs)
  where
    mean = sum xs / fromIntegral (length xs)
    squaredDifferences = map (\x -> (x - mean) ^ 2) xs

-- Exercise 4
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = [x | x <- xs, not (x `elem` ys)]

-- Exercise 5
splits :: (Ord a) => [a] -> [([a], [a])]
splits [] = []

-- Exercise 6
argmin :: (Ord a) => (t -> a) -> [t] -> t
argmin f (x:xs) = argmin' f xs x (f x)
  where
    argmin' _ [] minElem _ = minElem
    argmin' f (x:xs) minElem minValue
      | f x < minValue = argmin' f xs x (f x)
      | otherwise = argmin' f xs minElem minValue

-- Exercise 8
church :: Int -> (c -> c) -> c -> c
church n f x = foldr (\_ acc -> f acc) x [1..n]

-- Exercise 9
data BTree a = Leaf' a
  | Fork' (BTree a) (BTree a) deriving (Show, Eq)

--trees :: (Ord t) => [t] -> [Btree t]

-- Exercise 10
type Genome = String
insertions :: Genome -> [Genome]
insertions [] = []
insertions (x:xs) = map (\y -> y:x:xs) "AGCT" ++ map (x:) (insertions xs)

deletions :: Genome -> [Genome]
deletions [] = []
deletions (x:xs) = xs : map (x:) (deletions xs)

substitutions :: Genome -> [Genome]
substitutions [] = []
substitutions (x:xs) = map (\y -> y:xs) "AGCT" ++ map (x:) (substitutions xs)

transpositions :: Genome -> [Genome]
transpositions [] = []
transpositions [_] = []
transpositions (x:y:xs) = (y:x:xs) : map (x:) (transpositions (y:xs))