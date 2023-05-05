type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

-- A completely new type can be defined by speicifying its values 
-- using a data declaration

data Bool = False | True
-------
data Answer = Yes | No | Unknown
answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown


--data Maybe a = Nothing | Just a

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide m n = Just (m `div` n)
-- so that dividing by 0 doesn't crash the program
-- or giving head nothing for having an empty list doesn't crash the program
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
---------------------------------------------

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero   = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


--------------TREEE ------------------------------

data BTree a = Leaf a | Fork' (BTree a) (BTree a) deriving (Show, Eq)

foo = Fork (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) (Leaf 'd')

-- how to find the height of the tree
height :: BTree a -> Int
height (Leaf _) = 0
height (Fork' xt yt) = height xt `max` height yt + 1

-- how about size? the number of leaves
size :: BTree a -> Int
size (Leaf _) = 1
size (Fork' xt yt) = size xt + size yt




makeBTree :: [a] -> BTree a
makeBTree [x] = Leaf x
makeBTree xs = Fork' (makeBTree (take m xs )) (makeBTree (drop m xs))
    where m = length xs `div` 2



foldBTree :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldBTree f g (Leaf x) = f x
foldBTree f g (Fork' xt yt) = g (foldBTree f g xt) (foldBTree f g yt)

-- abstracted size
size' = foldBTree (\_ -> 1) (+) 

height' = foldBTree (\_ -> 0) (\x y -> x `max` y + 1)

-- flatten b tree
flatten :: BTree a -> [a]
flatten (Leaf x) = [x]
flatten (Fork' xt yt) = flatten xt ++ flatten yt

flatten' = foldBTree (\x -> [x]) (++)

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (Leaf x) = Leaf (f x)
mapBTree f (Fork' xt yt) = Fork' (mapBTree f xt) (mapBTree f yt)

mapBTree' f = foldBTree (Leaf . f) Fork'

