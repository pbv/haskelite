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

-- convert the stack into a partial expression;
-- aborts if we reach a guard/case alternative
unwindStack : Stack -> Expr -> (Stack, Expr)
unwindStack stack acc
    = case stack of
          (Update _::rest) ->
              unwindStack rest acc
          (PushArg arg::rest) ->
              unwindStack rest (App acc arg)
          (ContBinary1 op e2::rest) ->
              unwindStack rest (BinaryOp op acc e2)
          (ContBinary2 op e1::rest) ->
              unwindStack rest (BinaryOp op e1 acc) 
          (RetBinary op _ _ ::rest) ->
              unwindStack rest acc
          (ContUnary op::rest) ->
              unwindStack rest (UnaryOp op acc)
          (RetUnary op _ ::rest) ->
              unwindStack rest acc              
          (MatchEnd::rest) ->
              unwindStack rest acc
          (DeepEval::rest) ->
              unwindStack rest acc
          (Continue expr ctx::rest) ->
              unwindStack rest (ctx.set acc expr)
          _ ->
              (stack, acc)
