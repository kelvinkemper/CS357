-- Commonly used functions to know
-- head ls ==> fist element of the list
-- tail ls ==> cdr of list
-- !! ==> nth element of the list (like list.get(i)) where use list !! i

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns
average' ns = div (sum ns) (length ns)

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last' xs = head (reverse xs)
last'' xs = xs !! (length xs - 1)

init' xs = reverse (tail (reverse xs))

init'' xs = take (length xs -1) xs