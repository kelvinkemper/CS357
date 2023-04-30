-- Kelvin Kemper
-- Week 13 Recitation
data BinInt = Empty | Zero BinInt | One BinInt
-- data Nat = Zero | Succ Nat
-- Zero BinInt == 0 with digits

-- seven has the binary representation 111
-- seven = One (One (One Empty))

-- ...0000
zero :: BinInt
zero = Zero zero

-- ...0001
one :: BinInt
one = One zero

showFirstDigs :: Int -> BinInt -> String
showFirstDigs 0 _ = "..."
-- how to show the first n digits of a binary number to the right
showFirstDigs n (Zero b) = (showFirstDigs (n - 1) b) ++ "0"

--show first n digits of binint that starts with One
showFirstDigs n (One b) = (showFirstDigs (n - 1) b) ++ "1"

addone :: BinInt -> BinInt
addone (Zero b) = One b
addone (One b) = Zero (addone b)

natToBin :: Int -> BinInt
natToBin 0 = zero
natToBin n = addone (natToBin (n - 1))

plus :: BinInt -> BinInt -> BinInt
plus (Zero b1) (Zero b2) = Zero (plus b1 b2)
plus (Zero b1) (One b2) = One (plus b1 b2)
plus (One b1) (Zero b2) = One (plus b1 b2)
plus (One b1) (One b2) = addone (One (plus b1 b2))

instance Show BinInt where
    show b = showFirstDigs 20 b

negativeOne :: BinInt
negativeOne = One negativeOne

negativeTwo :: BinInt
negativeTwo = Zero negativeOne

negativeFour :: BinInt
negativeFour = plus negativeTwo negativeTwo

-- Kind of behaves like -1/3
-- because x + x + x = -1
wtf3 :: BinInt
wtf3 = One (Zero wtf3)

negativeThree :: BinInt
negativeThree = plus negativeOne negativeTwo

minusOne :: BinInt -> BinInt
minusOne (Zero b) = One (minusOne b)
minusOne (One b) = Zero b


-- Exercise 1: find "negative five"
-- i.e. define  negativeFive :: BinInt
-- such that plus (natToBin 5) negativeFive gives ...00000
negativeFive :: BinInt
negativeFive = plus negativeOne (plus negativeTwo negativeTwo)

-- Exercise 2: define "times" for these numbers
-- test cases: (times (natToBin 5) (natToBin 3)) ==> ...0001111
times :: BinInt -> BinInt -> BinInt
times (Zero b1) (Zero b2) = zero
times (Zero b1) (One b2) = zero
times (One b1) (Zero b2) = zero
times (One b1) (One b2) = (plus (plus b1 b1) b2)
--idk




