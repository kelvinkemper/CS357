--flatten :: BTree a -> [a]
--flatten (Leaf x) = [x]
--flatten (Fork xt yt) = flatten xt ++ flatten yt

-- eta reduction
-- \x -> f x => f
--flatten' = foldBtree (: []) (++)

--mapBTree :: (a -> b) -> BTree a -> BTree b
--mapBTree f (Leaf x) = Leaf (f x)
--mapBTree f (Fork xt yt) = Fork (mapBTree f xt) (mapBTree f yt)

-- b student
--mapBTree' f = foldBTree (\x -> Leaf (f x))

-- a student
--mapBTree' f = foldBTree (Leaf . f) Fork

--Rose Tree (variable numbers of children 0 to n)
-- has no leaves, only 1 data constructor
data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)

x = Node 5 [Node 1 [], Node 2 [Node 3[], Node 4 [Node 5 []]]]

--fringe of a rose tree
fringe :: RoseTree a -> [a]
fringe (Node x []) = [x]
fringe (Node x xs) = concatMap fringe xs

flattenRoseTree (Node x []) = [x]
flattenRoseTree (Node x xs) = x : concatMap fringe xs

sumRoseTree :: Num a => RoseTree a -> a
sumRoseTree (Node x []) = x
sumRoseTree (Node x xs) = x + sum $ map sumRoseTree xs