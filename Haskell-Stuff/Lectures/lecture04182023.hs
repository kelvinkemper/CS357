-- enum in Haskell is fun because you can use range syntax with the type
-- 

data Rational' = Rational' {numr :: Int, denr :: Int}

gcd' (x, y)
  | x == y = x
  | x < y = gcd' (x, y - x)
  | otherwise = gcd' (x - y, x)

(%) :: Int -> Int -> Rational'
x % y = let z = gcd' (x, y) in Rational' (x `div` z) (y `div` z)


instance Show Rational' where
    show (Rational' a 1) = show a
    show (Rational' a b) = show a ++ "/" ++ show b
    
instance Eq Rational' where
    (Rational' a b) == (Rational' c d) = a * d == b * c

instance Num Rational' where
    (Rational' a b) * (Rational' c d) = (a * c) % (b * d)
    (Rational' a b) + (Rational' c d) = (a * d + c * b) % (b * d)
    fromInteger x = fromIntegral x % 1

instance Fractional Rational' where
    (Rational' a b) / (Rational' c d) = (a * d) % (b * c)
-- fromRational (Rational a b) = Rational' (fromIntegral a) (fromIntegral b)

instance Ord Rational' where
    (Rational' a b) > (Rational' c d) = a * d > b * c
    (Rational' a b) < (Rational' c d) = a * d < b * c

instance Enum Rational' where
    
