module Machine.Types exposing (..)

import AST exposing (..)
import Context exposing (ExprCtx)
import Machine.Heap exposing (Heap)

type alias Conf
    = (Heap, Control, Stack)

type Control
    = E Expr
    | M Matching ArgStack

type alias ArgStack
    = List Expr
      
type alias Stack
    = List Cont

type Cont
    = PushArg Expr
    | Update Name
    | MatchEnd 
    | PushAlt ArgStack Matching
    | PushPat ArgStack Pattern Matching
      -- for bang patterns
    | PushBang ArgStack Matching
      -- continuations for primitive operations
    | ContBinary1 Name Expr
    | ContBinary2 Name Expr
    | ContUnary Name 
    | RetBinary Name Expr Expr 
    | RetUnary Name Expr 
      -- for full normal form evaluation
    | DeepEval
    | Continue Expr ExprCtx


      
getHeap : Conf -> Heap
getHeap (heap,_,_) = heap

getControl : Conf -> Control
getControl (_, control, _) = control

getStack : Conf -> Stack
getStack (_, _, stack) = stack           

-- check for final configuration
checkFinal : Conf -> Bool
checkFinal (_, _, stack) = List.isEmpty stack
