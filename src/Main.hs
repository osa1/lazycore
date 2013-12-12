module Main where


import Language
import Parser
import Heap
import TiStats
import PPrint

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Text.Parsec as P


runProg :: String -> String
runProg = showResults . eval . compile . parse

parse :: String -> CoreProgram
parse s = case P.parse program s s of
            Left err -> error (show err)
            Right p -> p

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
  -- fifth component is to accumulate stats

type TiStack = [Addr]

data TiDump = TiDump -- dummy value for now
initialTiDump = TiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int

type TiGlobals = M.Map Name Addr

compile :: CoreProgram -> TiState
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  where
    sc_defs = program ++ preludeDefs -- ++ extraPreludeDefs

    (initial_heap, globals) = buildInitialHeap sc_defs

    initial_stack = [address_of_main]

    address_of_main = case M.lookup "main" globals of
                        Nothing -> error "main is not defined"
                        Just m -> m

extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = iter sc_defs hInitial M.empty
  where
    iter :: [CoreScDefn] -> TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
    iter [] heap globals = (heap, globals)
    iter ((name, args, body) : defs) heap globals =
      let (heap', addr) = hAlloc heap (NSupercomb name args body)
          globals' = M.insert name addr globals in
      iter defs heap' globals'

eval :: TiState -> [TiState]
eval state = state : rest_states
  where
    rest_states
      | tiFinal state = []
      | otherwise     = eval next_state

    next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], _, heap, _, _) = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node     = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack) = map get_arg stack
  where
    get_arg addr = arg where (NAp _ arg) = hLookup heap addr

instantiate :: CoreExpr           -- Body of supercombinator
               -> TiHeap          -- Heap before instantiation
               -> M.Map Name Addr -- Association of names to addresses
               -> (TiHeap, Addr)  -- Heap after instantiation, and
                                  -- address of root of instance
instantiate (ENum n) heap _ = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, val)
  where
    val = case M.lookup v env of
            Nothing -> error $ "Undefined name " ++ show v
            Just val' -> val'
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) _ _ = error "Can't instantiate case exprs"

instantiateConstr tag arity heap env = error "Can't instantiate constructors yet"
instantiateLet False defs body heap env = iter defs heap env
  where
    iter [] heap' env' = instantiate body heap' env'
    iter ((name, rhs) : defs) heap' env' =
      let (heap'', addr) = instantiate rhs heap' env in
      iter defs heap'' (M.insert name addr env')

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
    if length stack < length arg_names
      then error "not enough arguments"
      else (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : (drop (length arg_names + 1) stack)
    (new_heap, result_addr) = instantiate body heap env
    env = M.union (M.fromList arg_bindings) globals
    arg_bindings = zip arg_names (getargs heap stack)

showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [ iLayn (map showState states), showStats (last states) ])

showState :: TiState -> Iseq
showState (stack, _, heap, _, _) = iConcat [ showStack heap stack, iNewline, showHeap heap, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack =
    iConcat [ iStr "Stk  ["
            , iIndent (iInterleave iNewline (map show_stack_item stack))
            , iStr " ]"
            ]
  where
    show_stack_item addr =
      iConcat [ showFWAddr addr, iStr ": "
              , showStkNode heap (hLookup heap addr)
              ]

showHeap :: TiHeap -> Iseq
showHeap (_, _, heap) =
    iConcat [ iStr "Heap ["
            , iIndent (iInterleave iNewline (map show_heap_item $ M.toList heap))
            , iStr " ]"
            ]
  where
    show_heap_item (addr, node) =
      iConcat [ showFWAddr addr
              , iStr ": "
              , showNode node
              ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) =
    iConcat [ iStr "NAp ", showFWAddr fun_addr,
              iStr " ", showFWAddr arg_addr, iStr " (",
              showNode (hLookup heap arg_addr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
    iConcat [ iStr "NAp ", showAddr a1,
              iStr " ", showAddr a2 ]
showNode (NSupercomb name args body) =
    iConcat [ iStr ("NSupercomb " ++ name)
            , iStr " [ ", iInterleave (iStr " ") (map iStr args), iStr " ]"
            ]
showNode (NNum n) = iStr "NNum " `iAppend` iNum n

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq -- Show address in field of width 4
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
    iConcat [ iStr "Total number of steps = ", iNum (tiStatGetSteps stats), iNewline ]
