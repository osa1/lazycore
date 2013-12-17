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
unitTests = testGroup "G-machine unit tests" $ map mkTestCase
    [ ("pgm1", pgm1, NNum 3)
    , ("pgm2", pgm2, NNum 3)
    , ("pgm3", pgm3, NNum 3)
    , ("pgm4", pgm4, NNum 3)
    , ("pgm5", pgm5, NNum 3)
    , ("pgm6", pgm6, NNum 4)
    , ("pgm7", pgm7, NNum 3)
    , ("pgm8", pgm8, NNum 4)
    , ("pgm9", pgm9, NNum 17)
    , ("pgm10", pgm10, NNum 8)
    , ("pgm11 (list len)", pgm11, NNum 3)
    , ("pgm12 (fac)", pgm12, NNum 120)
    , ("pgm13 (gcd)", pgm13, NNum 2)
    , ("pgm14 (fib)", pgm14, NNum 5)
    , ("pgm15 (infinite pair)", pgm15, NNum 4)
    , ("pgm16 (length nil with ADTs)", pgm16, NNum 0)
    , ("pgm17 (length lst with ADTs)", pgm17, NNum 3)
    , ("pgm18 (nth)", pgm18, NNum 2)
    , ("pgm19 (repeat)", pgm19, NNum 3)
    , ("weird (bug)", weird, NNum 6)
    , ("pgm20 (nested case)", pgm20, NNum 2)
    ]
  where
    mkTestCase (name, pgm, ret) = testCase name $ stackTop (runProg' pgm) @?= Right ret
