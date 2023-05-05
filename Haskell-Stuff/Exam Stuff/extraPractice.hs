takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if (p x) then x:takeWhile' p xs else []

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p [] = ([],[])
span' p xs = (takeWhile' p xs, dropWhile' p xs) 
    where
        dropWhile' p [] =[]
        dropWhile' p (y:ys) = if p y then dropWhile' p ys else y:ys

data Maybe a = Nothing | Just a
findKey :: (Eq a) => a -> [(a,b)] -> Maybe b
findKey key [] = Nothing
findKey key ((x,y):xs) = if key == x then Just y else findKey key xs

findKey' :: (Eq a) => a -> [(a, b)] -> Maybe b
findKey' key xs = case [v | (k, v) <- xs, k == key] of
  [] -> Nothing
  (v:_) -> Just v

splitText :: (Char -> Bool) -> String -> [String]
splitText _ [] = []
splitText p xs
  | null prefix = splitText p (dropWhile (not . p) suffix)
  | otherwise = prefix : splitText p suffix
  where
    (prefix, suffix) = span p (dropWhile (not . p) xs)

findKey'' key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing
