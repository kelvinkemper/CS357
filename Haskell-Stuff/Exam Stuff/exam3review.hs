
f1 :: (Int -> Int) -> Int
-- takes an argument that is an int to int function and returns an int output
f1 g = g 0

f2 :: Int -> Int -> Int
-- takes 2 ints for it's type signature
-- ex:
f2 x y = x + y

futile :: (a -> Bool) -> (a -> a) -> a -> a
futile pred func x = if (pred x) then x else futile pred func (func x)

divide :: [a] -> Int -> [[a]]
divide xs n = if (length xs < n)
                    then [] 
                    else (take n xs):(divide (drop n xs) n)

--divide' :: [a] -> Int -> [[a]]
--divide' xs n = foldr (\x lls -> ) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred = foldr (\x ls -> if (pred x) then x:ls else []) []

mySpan :: (a -> Bool) -> [a] -> ([a],[a])
mySpan pred = foldr (\x (ls1,ls2) -> if (pred x) 
                        then (x:ls1,ls2)
                        else ([],x:(ls1 ++ ls2)))
                        ([],[])