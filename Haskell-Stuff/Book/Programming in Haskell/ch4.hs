-- Defining Functions

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip x = 1/x

-- Conditional expressions -- 
-- using form 
        -- something = if (expr) then (true) else (false)
    --ex:
abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- Pattern matching --
--  If the first pattern is matched , then the first result is chosen;
--  otherwise, if the second is matched, then the second result is chosen,
--  and so on example,

--(&&) :: Bool -> Bool -> Bool
--True && True   = True
--True && False  = False
--False && True  = False
--False && False = False

-- using the wildcard pattern _ makes this even simpler
(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False


head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs


add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)


