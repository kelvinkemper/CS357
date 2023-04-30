addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float  
circumference r = 2 * pi * r  


-- use :t to check the types, example :t fst, :t 'a'
-- more exmpales: :t True, :t (True, 'a'), :t "Hello",

--Typeclasses , example to check like  :t (+), :t (==)

-- Eq is used for types that support equality testing
-- Ord is for types that have an ordering such as (>)

-- sometimes we'll need to receive a specific type
-- use type annotations
-- read "5" :: Int ==> 5
-- read "5" :: Float ==> 5.0
-- (read "5" :: Float) * 4  ==> 20.0
-- read "[1,2,3,4]" :: [Int] ==> [1,2,3,4]
-- read "(3, 'a')" :: (Int, Char) ==> (3, 'a')