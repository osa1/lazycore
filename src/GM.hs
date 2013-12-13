{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module GM where


import Language
import Parser
import PPrint
import Heap
import Programs

import qualified Text.Parsec as P
import qualified Data.Map.Strict as M

--import Debug.Trace


runProg :: String -> String
runProg = showResults . eval . compile . parse

compileProg :: String -> String
compileProg = iDisplay . showState . compile . parse

parse :: String -> CoreProgram
parse s = case P.parse program s s of
            Left err -> error (show err)
            Right p -> p

type GmState
    = (GmCode,      -- Current instruction stream
       GmStack,     -- Current stack
       GmHeap,      -- Heap of nodes
       GmGlobals,   -- Global addresses in heap
       GmStats)     -- Statistics

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, _, _, _, _) = i

putCode :: GmCode -> GmState -> GmState
putCode i (_, stack, heap, globals, stats) = (i, stack, heap, globals, stats)

data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Update Int
    | Pop Int
    | Alloc Int
    | Slide Int
    deriving ( Show, Eq )

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (_, stack, _, _, _) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack (i, _, heap, globals, stats) = (i, stack, heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (_, _, heap, _, _) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap (i, stack, _, globals, stats) = (i, stack, heap, globals, stats)

data Node
    = NNum Int              -- Numbers
    | NAp Addr Addr         -- Applications
    | NGlobal Int GmCode    -- Globals
    | NInd Addr             -- Indirections
    deriving ( Show, Eq )

type GmGlobals = M.Map Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (_, _, _, globals, _) = globals

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncStep :: GmStats -> GmStats
statIncStep = (+) 1

statGetSteps :: GmStats -> Int
statGetSteps = id

getStats :: GmState -> GmStats
getStats (_, _, _, _, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats (i, stack, heap, globals, _) = (i, stack, heap, globals, stats)


--------------------------------------------------------------------------------


--
-- Compiler
--

compile :: CoreProgram -> GmState
compile pgm = (initialCode, [], heap, globals, statInitial)
  where (heap, globals) = buildInitialHeap compiled hInitial M.empty

        buildInitialHeap :: [GmCompiledSC] -> GmHeap -> GmGlobals -> (GmHeap, GmGlobals)
        buildInitialHeap [] h g = (h, g)
        buildInitialHeap (p : ps) h g =
          let (h', (name, addr)) = allocateSc h p
              g' = M.insert name addr g in
          buildInitialHeap ps h' g'

        compiled = map compileSc (preludeDefs ++ pgm) ++ compiledPrimitives

type GmCompiledSC = (Name, Int, GmCode)

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (M.fromList $ zip env [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = M.Map Name Int

compileR :: GmCompiler
compileR e env = compileC e env ++ [Update (M.size env), Pop (M.size env), Unwind]

compileC :: GmCompiler
compileC (EVar v) env =
    case M.lookup v env of
      Nothing -> [Pushglobal v]
      Just n  -> [Push n]
compileC (ENum n) _ = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet recursive defs e) env
  | recursive = compileLetrec defs e env
  | otherwise = compileLet    defs e env

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = M.map (+ n) env

compileLet :: [(Name, CoreExpr)] -> GmCompiler
compileLet defs expr env = compileLet' defs env ++ compileC expr env' ++ [Slide (length defs)]
  where
    env' = mkLetEnv defs env

    compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
    compileLet' [] _ = []
    compileLet' ((_, expr) : defs) env = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetrec :: [(Name, CoreExpr)] -> GmCompiler
compileLetrec defs expr env = [Alloc n] ++ compDefs ++ compileC expr env' ++ [Slide n]
  where
    env' = mkLetEnv defs env
    n = length defs

    compDefs :: GmCode
    compDefs = concat $ map (\(i, def) -> compileC def env' ++ [Update i]) (zip [n-1, n-2 .. 0] (map snd defs))

mkLetEnv :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
mkLetEnv defs env = M.fromList (zip (map fst defs) [n-1, n-2 .. 0]) `M.union` argOffset n env
  where n = length defs

--
-- Interpreter
--

eval :: GmState -> [GmState]
eval state = state : restStates
  where
    restStates
      | gmFinal state = []
      | otherwise     = eval nextState

    nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncStep (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = case getCode s of
              [] -> True
              _  -> False

step :: GmState -> GmState
step state =
    let new_state = dispatch i (putCode is state) in
    --trace ("new stack after: " ++ iDisplay (showInstruction i) ++ "\n" ++ iDisplay (showStack new_state)) new_state
    new_state
  where
    (i : is) = getCode state

dispatch :: Instruction -> GmState -> GmState

dispatch (Pushglobal name) state = putStack (a : getStack state) state
  where a = case M.lookup name (getGlobals state) of
              Nothing -> error $ "Undeclared global " ++ name
              Just v  -> v

dispatch (Pushint n) state = putHeap heap' (putStack (addr : getStack state) state)
  where (heap', addr) = hAlloc (getHeap state) (NNum n)

dispatch Mkap state = putHeap heap' (putStack (addr : as') state)
  where (heap', addr)   = hAlloc (getHeap state) (NAp a1 a2)
        (a1 : a2 : as') = getStack state

dispatch (Push n) state = putStack (a : as) state
  where as = getStack state
        a  = as !! n

dispatch (Update n) state = putStack as (putHeap (hUpdate heap (as !! n) (NInd a)) state)
  where (a : as) = getStack state
        heap = getHeap state

dispatch (Pop n) state = putStack (drop n (getStack state)) state

dispatch Unwind state = newState (hLookup heap a)
  where stack@(a : as) = getStack state
        heap           = getHeap state

        newState (NInd n) = putCode [Unwind] (putStack (n : as) state)
        newState NNum{} = state
        newState (NAp a1 _) = putCode [Unwind] (putStack (a1 : a : as) state)
        newState (NGlobal n c)
          | length as < n = error $
              concat [ "Unwinding with too few arguments. "
                     , show n, " - ", show c ]
          | otherwise     = putCode c (putStack (rearrange n heap stack) state)

        rearrange :: Int -> GmHeap -> GmStack -> GmStack
        rearrange n heap as = take n as' ++ drop n as
          where as' = map (getArg . hLookup heap) (tail as)

        getArg :: Node -> Addr
        getArg (NAp _ a2) = a2
        getArg n = error $ "getArg of " ++ show n

dispatch (Alloc n) state = putStack (new_addrs ++ stack) (putHeap new_heap state)
  where
    heap = getHeap state
    stack = getStack state

    (new_heap, new_addrs) = allocNodes n heap

    allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
    allocNodes 0 heap = (heap, [])
    allocNodes n heap = (heap2, a : as)
      where (heap1, as) = allocNodes (n-1) heap
            (heap2, a)  = hAlloc heap1 (NInd hNull)

dispatch (Slide n) state = putStack (a : drop n as) state
  where (a : as) = getStack state


--
-- Printer
--

showResults :: [GmState] -> String
showResults states = iDisplay $ iConcat
    [ iStr "Supercombinator definitions", iNewline,
      iInterleave iNewline (map (showSC s) (M.toList $ getGlobals s)),
      iNewline, iStr "State transitions", iNewline,
      iLayn (map showState states),
      showStats (last states) ]
  where (s : _) = states

showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr) = iConcat
    [ iStr "Code for ", iStr name, iNewline,
      showInstructions code, iNewline ]
  where (NGlobal _ code) = hLookup (getHeap s) addr

showInstructions :: GmCode -> Iseq
showInstructions is = iConcat
    [ iStr "  Code:{",
      iIndent (iInterleave iNewline (map showInstruction is)),
      iStr "}", iNewline ]

showInstruction :: Instruction -> Iseq
showInstruction = iStr . show

showState :: GmState -> Iseq
showState s = iConcat
    [ showStack s, iNewline,
      showInstructions (getCode s), iNewline ]

showStack :: GmState -> Iseq
showStack s = iConcat
    [ iStr " Stack:[",
      iIndent (iInterleave iNewline
                (map (showStackItem s) (getStack s))),
      iStr "]" ]

showStackItem :: GmState -> Addr -> Iseq
showStackItem s a = iConcat
    [ iStr (showaddr a), iStr ": ",
      showNode s a (hLookup (getHeap s) a) ]

showNode :: GmState -> Addr -> Node -> Iseq
showNode _ _ (NNum n) = iNum n
showNode s a (NGlobal ar _) = iConcat [ iStr "Global ", iStr v, iStr "[", iNum ar, iStr "]" ]
  where v = head [n | (n, b) <- M.toList (getGlobals s), a == b]
showNode _ _ (NAp a1 a2) = iConcat [ iStr "Ap ", iStr (showaddr a1),
                                     iStr " ", iStr (showaddr a2) ]
showNode _ _ (NInd n) = iConcat [ iStr "#", iNum n ]

showStats :: GmState -> Iseq
showStats s = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s)), iNewline ]
