{-
  Pretty-printer for Haskelite expressions, types
  and machine configurations
  Pedro Vasconcelos 2021-23
-}
module Pretty exposing (..)

import AST exposing (Expr(..), Matching(..), Pattern(..), Name)
import Types exposing (Type(..))
import Heap exposing (Heap)
import Machine exposing (Conf, Stack, Control(..), Cont(..))
import Dict
import DList exposing (DList)

-- a type for strings with efficient concatenation
-- i.e. difference lists of strings
type alias StringBuilder
    = DList String

-- convert a string builder back into a string      
toString : StringBuilder -> String
toString sb
    = String.concat (DList.toList sb)

-- optionaly add parenthesis around a stringbuilder      
paren : Bool -> StringBuilder -> StringBuilder
paren cond sb
    = if cond then
          bracket "(" ")" sb
      else
          sb

-- bracket a stringbuild with start and end delimiters
bracket : String -> String -> StringBuilder -> StringBuilder
bracket start end sb
    = DList.cons start (DList.append sb (DList.singleton end))


-- toplevel function for pretty printing expressions 
prettyExpr : Heap -> Expr -> String
prettyExpr h e = toString (prettyExpr_ h 0 e)

prettyExpr_ : Heap -> Int ->  Expr -> StringBuilder
prettyExpr_ heap prec e =
    case e of
        Number n ->
            paren (prec>0 && n<0) <| DList.singleton (String.fromInt n)

        Var x ->
            if Heap.isIndirection x then
                case Dict.get x heap of
                    Just e1 ->
                        prettyExpr_ heap prec e1
                    Nothing ->
                        DList.singleton x -- this should not happen!
            else
                paren (AST.isOperator x) (DList.singleton x)

        Cons "," args ->
            bracket "(" ")" <|
                 (DList.intersperse
                      (DList.singleton ",")                      
                      (List.map (prettyExpr_ heap 0) args))


        Cons ":" [e1, e2] ->
            paren (prec>0)
                (DList.append (prettyExpr_ heap 1 e1)
                     (DList.cons ":" (prettyExpr_ heap 1 e2)))

        Cons tag [] ->
            DList.singleton tag
                    
        Cons tag args ->
            paren (prec>0) <|
                (DList.intersperse (DList.singleton " ")
                         ((DList.singleton tag) ::
                          (List.map (prettyExpr_ heap 1) args)))

        InfixOp op e1 e2 ->
            paren (prec>0)
                <| DList.append (prettyExpr_ heap 1 e1)
                    (DList.append (DList.singleton (formatOperator op))
                                       (prettyExpr_ heap 1 e2))

        App (App (Var op) e1) e2 ->
            if AST.isOperator op then
                paren (prec>0) <|
                    DList.append (prettyExpr_ heap 1 e1)
                        (DList.cons (" " ++ op ++ " ")
                             (prettyExpr_ heap 1 e2))
            else
                paren (prec>0) <|
                    DList.append (prettyExpr_ heap 0 (App (Var op) e1))
                        (DList.append (DList.singleton " ") (prettyExpr_ heap 1 e2))
                        
        App e0 e1 ->
            paren (prec>0) <|
            DList.append (prettyExpr_ heap 0 e0)
                (DList.append (DList.singleton " ") (prettyExpr_ heap 1 e1))


        Lam optid match ->
            case collectArgs match [] of
                (_, []) ->
                    prettyLam heap prec optid match
                (match1, args1) ->
                    let
                        expr1 = List.foldl (\x y->App y x) (Lam optid match1)  args1
                    in 
                        prettyExpr_ heap prec expr1
                    

        Error ->
            DList.singleton "<runtime error>"

        _ ->
            DList.singleton "<unimplemented>"

{-                
        App (Var "enumFrom") e1 ->
            "[" ++ prettyExpr_ 1 e1 ++ "..]"

        App (App (Var "enumFromThen") e1) e2 ->
            "[" ++ prettyExpr_ 1 e1 ++ "," ++ prettyExpr_ 1 e2 ++ "..]"
                
        App (App (Var "enumFromTo") e1) e2 ->
            "[" ++ prettyExpr_ 1 e1 ++ ".." ++ prettyExpr_ 1 e2 ++ "]"

        App (App (App (Var "enumFromThenTo") e1) e2) e3 ->
            "[" ++ prettyExpr_ 1 e1 ++ "," ++ prettyExpr_ 1 e2 ++ ".."
                ++ prettyExpr_ 1 e3 ++ "]"

              
        IfThenElse e1 e2 e3 ->
            paren (prec>0)
                <|  "if " ++ prettyExpr e1 ++ " then " ++
                    prettyExpr e2 ++ " else " ++ prettyExpr e3

-}

collectArgs : Matching -> List Expr -> (Matching, List Expr)
collectArgs m args
    = case m of
          Arg e1 m1 ->
              collectArgs m1 (e1::args)
          _ ->
              (m, args)


-- pretty print a lambda
prettyLam : Heap -> Int -> Maybe Name -> Matching -> StringBuilder
prettyLam heap prec optid m
    = case optid of
          Just id ->
              -- just use the binding name if there is one
              DList.singleton id
          Nothing ->
              -- otherwise check if it has any arguments
              case m of
                  Match p m1 ->
                      paren True <| DList.cons "\\" (prettyMatch heap m)
                  Return e _ ->
                      prettyExpr_ heap prec e
                  _ ->
                      DList.singleton "<unimplemented>"
          
-- pretty print a matching            
prettyMatch : Heap -> Matching -> StringBuilder
prettyMatch heap m =
    case m of
        (Match p m1) ->
            DList.append (prettyPattern p)
                (DList.cons " " (prettyMatch heap m1))
        (Return e _) ->
            DList.cons "-> " (prettyExpr_ heap 0 e)

        _ ->
            DList.singleton "<unimplemented>"

-- format an infix operator, sometimes with spaces either side
formatOperator : Name -> String
formatOperator op
    = if op=="&&" || op == "||"
      then " " ++ op ++ " "
      else if AST.isOperator op then op else "`" ++ op ++ "`"
              
-- pretty print a pattern                   
prettyPattern : Pattern -> StringBuilder
prettyPattern p =
    case p of
        DefaultP ->
            DList.singleton "_"
        VarP x ->
            DList.singleton x
        BangP x ->
            DList.singleton ("!"++x)
        NumberP n ->
            DList.singleton (String.fromInt n)
                
        ConsP "," ps ->
            bracket "(" ")" <|
                DList.intersperse
                    (DList.singleton ",")
                    (List.map prettyPattern ps)

        ConsP ":" [p1,p2] ->
            bracket "(" ")" <|
                DList.append (prettyPattern p1)
                    (DList.cons ":" (prettyPattern p2))
        ConsP tag [] ->
            DList.singleton tag
        ConsP tag ps ->
            bracket "(" ")" <|
                DList.intersperse (DList.singleton " ")
                    ((DList.singleton tag) :: (List.map prettyPattern ps))
                
           

prettyType : Type -> String
prettyType ty = toString (prettyType_ 0 ty)

prettyType_ : Int -> Type -> StringBuilder
prettyType_ prec ty
    = case ty of
          TyInt ->
              DList.singleton "Int"
          TyBool ->
              DList.singleton "Bool"
          TyVar name ->
              DList.singleton name
          TyGen idx ->
              DList.singleton (showGenVar idx)
          TyList ty1 ->
              bracket "[" "]" (prettyType_ 0 ty1)
          TyTuple ts ->
              bracket "(" ")" <|
                  DList.intersperse
                      (DList.singleton ",")
                      (List.map (prettyType_ 0) ts) 
          TyFun t1 t2 ->
              paren (prec>0) <|
                  DList.append (prettyType_ 1 t1)
                      (DList.cons "->" (prettyType_ 0 t2))

showGenVar : Int -> String
showGenVar n
    = String.fromChar <| Char.fromCode <| Char.toCode 'a' + n

----------------------------------------------------------------------------------
-- showing configurations 
----------------------------------------------------------------------------------
                  
prettyConf : Conf ->  Maybe String
prettyConf (heap, control, stack)
   = case (control, stack) of
         (E expr, _) ->
             Just <| prettyCont heap stack expr
         _ ->
             Nothing
             

                 
-- convert a continuation stack into a string
prettyCont : Heap -> Stack -> Expr -> String
prettyCont heap stack acc
    = case stack of
          [] ->
              prettyExpr heap acc
          (Update _::rest) ->
              prettyCont heap rest acc
          (PushArg arg::rest) ->
              prettyCont heap rest (App acc arg)
          (RetPrim1 op e2::rest) ->
              prettyCont heap rest (InfixOp op acc e2)
          (RetPrim2 op v::rest) ->
              prettyCont heap rest (InfixOp op (Number v) acc)
          MatchEnd::rest ->
              prettyCont heap rest acc
          DeepEval expr ctx::rest ->
              prettyCont heap rest (ctx.set acc expr)
          (_::rest) ->
              "... " ++ prettyExpr heap acc

