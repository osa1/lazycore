module State where

type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
                   then [state]
                   else state : evalMult (stepMult state)

stepMult (n, m, d, t)
  | d > 0  = (n, m,   d-1, t+1)
  | d == 0 = (n, m-1, n,   t)

multFinal :: MultState -> Bool
multFinal (n, m, d, t) = m == 0 && d == 0
