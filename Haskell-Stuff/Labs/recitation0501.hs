-- Kelvin Kemper

-- user defined data types

-- (a, b)
data And a b = And a b deriving (Eq, Show)

data Or a b = OrL a | OrR b deriving (Eq, Show)


-- To construct elements of type Or Int Bool:
-- Or Int Bool
-- OrL 1
-- OrL 1000
-- OrR True
-- OrR False

reversePair :: (a, b) -> (b, a)
reversePair (x, y) = (y, x)

-- "(a and b) implies (b and a)"
andSwap :: And a b -> And b a
andSwap (And x y) = And y x


-- "(a and b) implies a"
andFirst :: And a b -> a
andFirst (And x y) = x

andSecond :: And a b -> b
andSecond (And x y) = y

-- (a , (b, c)) -> ((a, b), c)

-- "(a and (b and c)) implies ((a and b) and c)"
andLTR :: And a (And b c) -> And (And a b) c
andLTR (And x (And y z)) = And (And x y) z

-- "(a or b) implies (b or a)"
orSwap :: Or a b -> Or b a
orSwap (OrL x) = OrR x
orSwap (OrR y) = OrL y


-- x has type a
-- 
orLTR :: Or a (Or b c) -> Or (Or a b) c
orLTR (OrL x) = OrL (OrL x)
orLTR (OrR (OrL y)) = OrL (OrR y)
orLTR (OrR (OrR z)) = OrR z


-- "a and (b or c) implies (a and b) or (a and c)"
deMorganL :: And a (Or b c) -> Or (And a b) (And a c)
deMorganL (And x (OrL y)) = OrL (And x y)
deMorganL (And x (OrR z)) = OrR (And x z)

cases :: (a -> a') -> (b -> b') -> Or a b -> Or a' b'
cases f g (OrL x) = OrL (f x)
cases f g (OrR y) = OrR (g y)

-- Exercise 1
orRTL4 :: Or a (Or b (Or c d)) -> Or (Or (Or a b) c) d
orRTL4 (OrL w) = OrL (OrL (OrL w))
orRTL4 (OrR (OrL x)) = OrL (OrL (OrR x))
orRTL4 (OrR (OrR (OrL y))) = OrL (OrR y)
orRTL4 (OrR (OrR (OrR z))) = OrR z


-- Or1L a, Or1R (Or b (Or c d))
-- Or2L b, Or2R (Or c d)
-- Or3L c, Or3R d

-- Exercise 2
-- NO PATTERN MATCHING
-- Use only deMorganL, andSwap, cases, (.)
--deMorganR :: And (Or a b) c -> Or (And a c) (And b c)
--deMorganR deMorganL = andSwap . cases