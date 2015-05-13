module CoreLang.TInst
(
  eval
, compile
, showResults
)
where

import CoreLang.Utils
import CoreLang.Language
import CoreLang.ISeq
import Data.Maybe (fromMaybe)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]

data TiDump = DummyTiDump

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int

type TiGlobals = [(Name, Addr)]

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0
tiStatIncStep :: TiStats -> TiStats
tiStatIncStep s = s+1
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, scDefs, stats) =
  (stack, dump, heap, scDefs, f stats)

compile :: CoreProgram -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where scDefs = program ++ preludeDefs ++ extraPreludeDefs
        (initialHeap, globals) = buildInitialHeap scDefs
        initialStack = [addrOfMain]
        addrOfMain = fromMaybe (error "main is not defined") $ lookup "main" globals

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) =
  (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NSupercomb name args body)

eval :: TiState -> [TiState]
eval state = state : restStates
  where restStates | tiFinal state = []
                   | otherwise = eval nextState
        nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncStep state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], dump, heap, globals, stats) =
  isDataNode (hLookup heap soleAddr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
  where (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
  (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body =
  (newStack, dump, newHeap, globals, stats)
  where newStack = resultAddr : (drop (length argNames+1) stack)
        (newHeap, resultAddr) = instantiate body heap env
        env = argBindings ++ globals
        argBindings = zip argNames (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) =
  map getArg stack
  where getArg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env =
  hAlloc heap2 (NAp a1 a2)
  where (heap1, a1) = instantiate e1 heap env
        (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
  (heap, fromMaybe (error $ "Undefined name " ++ show v) $ lookup v env)
instantiate (EConstr tag arity) heap env =
  instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env =
  instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env =
  error "Can't instantiate case exprs"

instantiateConstr tag arity heap env =
  error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env =
  error "Can't instantiate let(recs)s yet"


-- |Converts a list of 'TiState' into formatted string.
showResults :: [TiState] -> String
showResults states =
  iDisplay $ iConcat [ iLayn (map showState states)
                     , showStats (last states) ]

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) =
  iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack =
  iConcat [
    iStr "Stk [",
    iIndent (iInterleave iNewline (map showStackItem stack)),
    iStr "]"
  ]
  where
  showStackItem addr =
    iConcat [ showFWAddr addr, iStr ": ",
              showStkNode heap (hLookup heap addr) ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp funAddr argAddr) =
  iConcat [ iStr "NAp ", showFWAddr funAddr,
            iStr " ", showFWAddr argAddr, iStr " (",
            showNode (hLookup heap argAddr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat [ iStr "NAp ", showAddr a1,
            iStr " ", showAddr a2 ]
showNode (NSupercomb name args body) =
  iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr $ show addr

showFWAddr :: Addr -> Iseq
showFWAddr addr =
  iStr (space (4 - length str) ++ str)
  where str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
  iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
            iNum (tiStatGetSteps stats) ]
