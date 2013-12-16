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

runProg' :: String -> GmState
runProg' = eval' . compile . parse

compileProg :: String -> String
compileProg = iDisplay . showCompileState . compile . parse

parse :: String -> CoreProgram
parse s = case P.parse program s s of
            Left err -> error (show err)
            Right p -> p

--
-- Types
--

data GmState = GmState
    { getCode :: GmCode         -- Current instruction stream
    , getStack :: GmStack       -- Current stack
    , getDump :: GmDump         -- Current dump
    , getHeap :: GmHeap         -- Heap of nodes
    , getGlobals :: GmGlobals   -- Global addresses in heap
    , getStats :: GmStats       -- Statistics
    } deriving ( Show )

type GmCode = [Instruction]
type GmStack = [Addr]
type GmHeap = Heap Node
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)
type GmGlobals = M.Map Name Addr
type GmStats = Int

data Node
    = NNum Int              -- Numbers
    | NAp Addr Addr         -- Applications
    | NGlobal Int GmCode    -- Globals
    | NInd Addr             -- Indirections
    deriving ( Show, Eq )

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
    | Eval
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | Cond GmCode GmCode
    deriving ( Show, Eq )

--
-- Setters
--

putCode :: GmCode -> GmState -> GmState
putCode i state = state{getCode = i}

putStack :: GmStack -> GmState -> GmState
putStack stack state = state{getStack = stack}

putHeap :: GmHeap -> GmState -> GmState
putHeap heap state = state{getHeap = heap}

putDump :: GmDump -> GmState -> GmState
putDump dump state = state{getDump = dump}

statInitial :: GmStats
statInitial = 0

statIncStep :: GmStats -> GmStats
statIncStep = (+) 1

statGetSteps :: GmStats -> Int
statGetSteps = id

putStats :: GmStats -> GmState -> GmState
putStats stats state = state{getStats = stats}


--------------------------------------------------------------------------------


--
-- Compiler
--

compile :: CoreProgram -> GmState
compile pgm = GmState initialCode [] [] heap globals statInitial
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
compiledPrimitives =
    [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
    , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
    , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
    , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
    , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
    , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
    , ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
    , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
    , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
    , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
    , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
    ]

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval]

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

eval' :: GmState -> GmState
eval' state = newState
  where
    newState
      | gmFinal state = state
      | otherwise     = eval' nextState

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
  where
    stack@(a : as) = getStack state
    heap           = getHeap state
    ((i, s) : ds)  = getDump state

    newState (NInd n) = putCode [Unwind] (putStack (n : as) state)
    newState NNum{}
      | length stack /= 1 = error $ "unwinding an int, but stack has more elements: " ++ iDisplay (showStack state)
      | length (getCode state) /= 0 = error $ "unwinding an int, but code part has more elements"
      | otherwise = putCode i (putStack (a : s) (putDump ds state))
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

dispatch Eval state = putCode [Unwind] (putStack [a] (putDump ((code, as) : dump) state))
  where
    (a : as) = getStack state
    code = getCode state
    dump = getDump state

dispatch Add state = arithmetic2 (+) state
dispatch Sub state = arithmetic2 (-) state
dispatch Mul state = arithmetic2 (*) state
dispatch Div state = arithmetic2 div state
dispatch Neg state = arithmetic1 (\i -> (-i)) state
dispatch Eq state = comparison (==) state
dispatch Ne state = comparison (/=) state
dispatch Lt state = comparison (<) state
dispatch Le state = comparison (<=) state
dispatch Gt state = comparison (>) state
dispatch Ge state = comparison (>=) state

dispatch (Cond t f) state = putStack as (putCode ((if condB then t else f) ++ i) state)
  where
    i = getCode state
    (a : as) = getStack state

    cond = hLookup (getHeap state) a
    condB = case cond of
              NNum 0 -> False
              NNum 1 -> True
              _ -> error "condition is not a number"


boxInteger :: Int -> GmState -> GmState
boxInteger n state = putStack (a : getStack state) (putHeap h' state)
  where
    (h', a) = hAlloc (getHeap state) (NNum n)

boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state = putStack (a : getStack state) (putHeap h' state)
  where
    (h', a) = hAlloc (getHeap state) (NNum (if b then 1 else 0))

unboxInteger :: Addr -> GmState -> Int
unboxInteger a state = ub (hLookup (getHeap state) a)
  where
    ub (NNum i) = i
    ub _        = error "Unboxing a non-integer"

primitive1 :: (b -> GmState -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> b)                  -- operator
           -> (GmState -> GmState)      -- state transition
primitive1 box unbox op state = box (op (unbox a state)) (putStack as state)
  where
    (a : as) = getStack state

primitive2 :: (b -> GmState -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> a -> b)             -- operator
           -> (GmState -> GmState)      -- state transition
primitive2 box unbox op state = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
  where
    (a0 : a1 : as) = getStack state

arithmetic1 :: (Int -> Int) -> (GmState -> GmState)
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

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
    [ showStack s,                  iNewline,
      showDump s,                   iNewline,
      showInstructions (getCode s), iNewline ]

showCompileState :: GmState -> Iseq
showCompileState s = iConcat
    [ showStack s,                  iNewline,
      showDump s,                   iNewline,
      showInstructions (getCode s), iNewline,
      iInterleave iNewline (map (showSC s) (M.toList $ getGlobals s)) ]

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

showDump :: GmState -> Iseq
showDump s = iConcat
    [ iStr "  Dump:[",
      iIndent (iInterleave iNewline (map showDumpItem (reverse (getDump s)))),
      iStr "]" ]

showDumpItem :: GmDumpItem -> Iseq
showDumpItem (code, stack) = iConcat
    [ iStr "<",
      shortShowInstructions 3 code, iStr ", ",
      shortShowStack stack,         iStr ">" ]

shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions number code = iConcat
    [ iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}" ]
  where
    codes = map showInstruction (take number code)
    dotcodes
      | length code > number = codes ++ [iStr "..."]
      | otherwise            = codes

shortShowStack :: GmStack -> Iseq
shortShowStack stack = iConcat
    [ iStr "[", iInterleave (iStr ", ") (map (iStr . showaddr) stack), iStr "]" ]

showStats :: GmState -> Iseq
showStats s = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s)), iNewline ]
