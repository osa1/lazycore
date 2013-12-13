module Programs where

pgm1, pgm2, pgm3, pgm4 :: String
pgm1 = "main = I 3"
pgm2 = "id = S K K ; main = id 3"
pgm3 = "id = S K K ; main = twice twice twice id 3"
pgm4 = "main = S K K 3"


testPgm :: String
testPgm = unlines
  [ "pair x y f = f x y ;"
  , "fst p = p K ;"
  , "snd p = p K1 ;"
  , "f x y = letrec"
  , "          a = pair x b ;"
  , "          b = pair y a"
  , "        in"
  , "        fst (snd (snd (snd a))) ;"
  , "main = f 3 4"
  ]
