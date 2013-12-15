module Programs where

pgm1 = "main = I 3"
pgm2 = "id = S K K ; main = id 3"
pgm3 = "id = S K K ; main = twice twice twice id 3"
pgm4 = "main = S K K 3"
pgm5 = "main = twice (I I I) 3"
pgm6 = unlines
  [ "cons a b cc cn = cc a b ;"
  , "nil      cc cn = cn ;"
  , "hd list = list K abort ;"
  , "tl list = list K1 abort ;"
  , "abort = abort ;"
  , "infinite x = cons x (infinite x) ;"
  , "main = hd (tl (infinite 4))"
  ]
pgm7 = "main = let id1 = I I I in id1 id1 3"
pgm8 = unlines
  [ "oct g x = let h = twice g"
  , "          in let k = twice h"
  , "          in k (k x) ;"
  , "main = oct I 4"
  ]
pgm9 = "main = (4 * 5) + (2 - 5)"
pgm10 = "inc x = x + 1 ; main = twice twice inc 4"
pgm11 = unlines $
  [ "cons a b cc cn = cc a b ;"
  , "nil      cc cn = cn ;"
  , "hd list = list K abort ;"
  , "tl list = list K1 abort ;"
  , "abort = abort ;"
  , "infinite x = cons x (infinite x) ;"
  ] ++
  [ "length xs = xs length1 0 ;"
  , "length1 x xs = 1 + (length xs) ;"
  , "main = length (cons 3 (cons 3 (cons 3 nil)))"
  ]
pgm12 = unlines
  [ "fac n = if (n == 0) 1 (n * fac (n-1)) ;"
  , "main = fac 5"
  ]
pgm13 = unlines
  [ "gcd a b = if (a == b) a (if (a < b) (gcd b a) (gcd b (a - b))) ;"
  , "main = gcd 6 10"
  ]
pgm14 = unlines
  [ "nfib n = if (n == 0) 0 (if (n == 1) 1 ((nfib (n-1)) + (nfib (n-2)))) ;"
  , "main = nfib 5"
  ]
pgm15 = unlines
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
