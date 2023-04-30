-- Kelvin Kemper
-- Lab Recitation 03-27-23

-- Activity
-- Write a function named 'myreverse'
-- that reverses a list. This should
-- have the following signature
-- myreverse :: [a] -> [a]

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

