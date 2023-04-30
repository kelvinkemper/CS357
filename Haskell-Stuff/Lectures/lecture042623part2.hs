import Prelude hiding ((>>=))

-- data Maybe a = Nothing | Just a

eToS = [("dog", "perro"), ("cat", "gato"), ("red", "rioja"), ("word", "palabra"), ("good","bueno"), ("head","cabeza")]
sToG = [("perro", "hund"), ("gato", "katz"), ("rioja", "rot"), ("palabra", "wort"), ("bueno", "gut"), ("cabeza", "kopf")]
gToF = [("hund", "chien"), ("katz", "chat"), ("rot", "rouge"), ("wort", "mot"), ("gut", "bon"), ("kopf", "tete")]
fToI = [("chien", "cane"), ("chat", "gotto"), ("rouge", "rosso"), ("mot", "parola"), ("bon", "buon"), ("tete", "testa")]

findKey :: Eq a => a -> [(a,b)] -> Maybe b
findKey key [] = Nothing
findKey key ((x,y):xs) = if key == x then Just y else findKey key xs

translate = flip findKey 