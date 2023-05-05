------------------------------------- ANOTHER TREE --------------------
data Tree a = Empty | Leaf a | Fork a (Tree a) (Tree a) deriving (Show)
bar = Fork 0 (Fork 1 (Fork 2 Empty (Leaf 3)) (Leaf 4)) (Fork 5 Empty (Leaf 6))