module Machine.Types exposing (..)

import AST exposing (Expr, Pattern, Matching, Name)
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
    | MatchEnd Skip
    | PushAlt ArgStack Matching
    | PushPat ArgStack Pattern Matching
      -- continuations for primitive operations
    | ContBinary Name Expr
    | RetBinary Name Expr
    | RetUnary Name
      -- for full normal form evaluation
    | DeepEval
    | Continue Expr ExprCtx

type Skip
    = Skipped | NotSkipped
