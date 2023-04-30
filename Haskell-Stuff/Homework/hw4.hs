-- Kelvin Kemper
-- CS 357 Homework 4
-- Due April 5th, 2023

-- Exercise 1
stutter :: [a] -> [a]
stutter [] = []
stutter (x:xs) = x : x : stutter xs


-- Exercise 2
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x : compress (y:xs)

-- Exercise 3
findIndices :: Num b => (a -> Bool) -> [a] -> [b]
findIndices p xs = go 0 xs []
  where go _ [] acc = reverse acc
        go i (x:xs) acc
          | p x = go (i+1) xs (i:acc)
          | otherwise = go (i+1) xs acc

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
  | x `elem` ys = x : intersect xs ys
  | otherwise = intersect xs ys

-- Exercise 4
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

-- Exercise 5
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs ys = xs == drop (length ys - length xs) ys

-- Exercise 6
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum $ zipWith (*) xs ys

-- Exercise 7
increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = x <= y && increasing (y:xs)

-- Exercise 8
decimate :: [a] -> [a]
decimate xs = map snd $ filter ((/= 0) . (`mod` 10) . fst) $ zip [1..] xs

-- Exercise 9
--encipher :: Eq a => [a] -> [a] -> [a] -> [a]
--encipher from to msg = map (\c -> maybe c id $ lookup c (zip from to)) msg

-- Exercise 10
prefixSum :: Num a => [a] -> [a]
prefixSum = scanl (+) 0

-- Exercise 11
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ [] _ = []
select _ _ [] = []
select p (x:xs) (y:ys)
  | p x = y : select p xs ys
  | otherwise = select p xs ys

-- Exercise 12
numbers :: [Integer] -> Integer
numbers xs =
  let
    go acc [] = acc
    go acc (y : ys) = go (10 * acc + y) ys
  in
    go 0 xs