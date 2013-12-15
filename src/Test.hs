module Main where

import GM
import Heap
import Programs

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Conc (getNumProcessors, setNumCapabilities)

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

stackTop :: GmState -> Either String Node
stackTop state
  | slen == 1 = Right $ hLookup heap (head stack)
  | otherwise = Left $ "stack has " ++ show slen ++ " elements."
  where
    stack = getStack state
    slen  = length stack
    heap  = getHeap state

unitTests :: TestTree
unitTests = testGroup "G-machine unit tests"
  [ testCase "pgm1" $ stackTop (runProg' pgm1) @?= Right (NNum 3)
  , testCase "pgm2" $ stackTop (runProg' pgm2) @?= Right (NNum 3)
  , testCase "pgm3" $ stackTop (runProg' pgm3) @?= Right (NNum 3)
  , testCase "pgm4" $ stackTop (runProg' pgm4) @?= Right (NNum 3)
  , testCase "pgm5" $ stackTop (runProg' pgm5) @?= Right (NNum 3)
  , testCase "pgm6" $ stackTop (runProg' pgm6) @?= Right (NNum 4)
  , testCase "pgm7" $ stackTop (runProg' pgm7) @?= Right (NNum 3)
  , testCase "pgm8" $ stackTop (runProg' pgm8) @?= Right (NNum 4)
  , testCase "pgm9 (simple arithmetic)" $ stackTop (runProg' pgm9) @?= Right (NNum 17)
  , testCase "pgm10" $ stackTop (runProg' pgm10) @?= Right (NNum 8)
  , testCase "pgm11 (list len)" $ stackTop (runProg' pgm11) @?= Right (NNum 3)
  , testCase "pgm12 (fac)" $ stackTop (runProg' pgm12) @?= Right (NNum 120)
  , testCase "pgm13 (gcd)" $ stackTop (runProg' pgm13) @?= Right (NNum 2)
  , testCase "pgm14 (fib)" $ stackTop (runProg' pgm14) @?= Right (NNum 5)
  , testCase "pgm15 (infinite pair)" $ stackTop (runProg' pgm15) @?= Right (NNum 4)
  ]
