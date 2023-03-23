ifThenElse x y z = if x then y else z

fact n = ifThenElse (n== 0) 1 (n * fact (n-1))

# lambda x is 
# \x
# (\x -> x + 1) 45

# map (\x -> x + 1) [1,2,3,4]
# map (const 2) ['a'..'z']

# type class constraints => type
# example: :t 5
# Num p => p

append xs ys = if null xs then ys else head xs : append (tail xs) ys