doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2


-- list comprehensions
-- [x*2 | x <- [1..10]] ==> [2,4,6,8,10,12,14,16,18,20]
-- adding a predicate, weeding specific values out is called FILTERING
-- [x*2 | x <- [1..10], x*2 >= 12] ==> [12,14,16,18,20] 

boombang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--multiple predicates
-- [ x*y | x <- [2,5,10], y <- [8,10,11]]  ==> [16,20,22,40,50,55,80,100,110] 
--same thing but we only want the numbers over 50
-- [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    -- [55,80,100,110] 

length' ls = sum [1 | _ <- ls]


removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

