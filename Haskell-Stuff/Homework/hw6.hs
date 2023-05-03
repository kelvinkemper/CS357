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
num2bits n = num2bits (n `div` 2) ++ show (n `mod` 2)

-- Exercise 3
variance :: Fractional a => [a] -> a
variance xs = sum squaredDifferences / fromIntegral (length xs)
  where
    mean = sum xs / fromIntegral (length xs)
    squaredDifferences = map (\x -> (x - mean) ^ 2) xs

-- Exercise 4
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = filter (`notElem` ys) xs

-- Exercise 5
splits _ = undefined

-- Exercise 6
argmin _ _ = undefined


-- Exercise 8
church :: Int -> (c -> c) -> c -> c
church n f x = foldr (\_ acc -> f acc) x [1..n]

-- Exercise 9
data BTree a = Leaf' a
  | Fork' (BTree a) (BTree a) deriving (Show, Eq)

trees _ = undefined

-- Exercise 10
insertions _ = undefined

deletions _ = undefined

substitutions _ = undefined

transpositions _ = undefined