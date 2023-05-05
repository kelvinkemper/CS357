-- user defined types and type classes
-- folds on trees and things like that
import Data.List (nub)
-- understand foldr
-- zip

length' [] = 0
length' (x:xs) = 1 + length' xs

delete' value [] = [] 
delete' value (x:xs) = if value == x then delete' value xs else x : delete' value xs

elem' x [] = False
elem' x (y:ys) = if x == y then True else elem' x ys

evens [] = []
evens (x:xs) = x : odds xs

odds [] = []
odds (x:xs) = evens xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

uncurry' f = \(a,b) -> f a b
curry' f = \a b -> f (a,b)

zip' [] [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = if (elem x xs) then True else duplicates xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if (p x) then x:takeWhile p xs else []

gaps :: (Enum t, Eq t) => [t] -> Bool
gaps xs = hasGaps xs || hasRepeats xs
  where
    hasGaps [] = False
    hasGaps [x] = False
    hasGaps (x:xs) = succ x /= head xs || hasGaps xs

    hasRepeats [] = False
    hasRepeats (x:xs) = x `elem` xs || hasRepeats xs



rot [] = []
rot [x] = [x]
rot (x:xs) = xs ++ [x]

rotate 0 xs = xs
rotate n xs = rotate (n-1) (rot xs)

church f n = (iterate (. f) id) !! n