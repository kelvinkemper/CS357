-- higher-order functions if it takes a function as an argument
-- or returns a function as a result
-- ex:
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- common high order functions

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a] 
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

filterLC :: (a -> Bool) -> [a] -> [a] 
filterLC p (x:xs) = [x | x <- xs, p x]

filterFoldr :: (a -> Bool) -> [a] -> [a] 
filterFoldr p (x:xs) = foldr step [] xs
    where
        step x acc = if p x then x : acc else acc


length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

lengthFoldr :: [a] -> Int
lengthFoldr = foldr (\_ n -> 1 + n) 0

reverseFoldr :: [a] -> [a]
reverseFoldr = foldr snoc []
    where
        snoc x xs = xs ++ [x]


