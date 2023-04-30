-- Final on Friday the 5th of May
-- Advanced topics in Haskell, no scheme

import System.Random

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show,Eq,Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq)

instance Ord Card where
    (Card x _) < (Card y _) = x < y
    (Card x _) >= (Card y _) = x >= y

instance Show Card where
    show (Card x y) = show x ++ show y

deck = [Card rank suit | rank <- [Two .. Ace], suit <- [Diamonds .. Clubs]]

riffle [] [] _ = []
riffle (x:xs) (y:ys) (0:flips) = x:y:riffle xs ys flips
riffle (x:xs) (y:ys) (1:flips) = y:x:riffle xs ys flips

riffleCut g xs = riffle (take 26 xs) (drop 26 xs) (randomRs (0 :: Int, 1 :: Int) g)

generators g0 = g1 : generators g2 where (g1, g2) = split g0

shuffle g = foldr1 (.) $ take 7 $ map riffleCut (generators g)

straight hand = [x /= Ace && y == succ x | (Card x _, Card y _) <- zip hand (tail hand)]

quickSort [] = []
quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

hands seed = [quickSort $ take 5 $ shuffle g deck | g <- generatos (mkStdGen seed)]

odds seed = fromIntegral (length $ filter straight $ take 10000 (hands seed)) / 100000


        
            
