foldr' _ seed [] = seed
foldr' f seed (x:xs) = f x : (foldr' f seed xs)


-- simple pattern of recursion:
-- f []     = value
-- f (x:xs) = x `infix op` f xs




sum []     = 0
sum (x:xs) = x + sum xs

product []     = 1
product (x:xs) = x + product xs

and []     = True
and (x:xs) = x && and xs 

-- General forms for fold right --
sum' = foldr (+) 0
product' = foldr (*) 1
or' = foldr (||) False
and' = foldr (&&) True

