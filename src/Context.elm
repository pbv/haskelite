{-
  Evaluation contexts for Haskelite expressions
  Pedro Vasconcelos, 2021
-} 
module Context exposing (..)

import AST exposing (Expr(..))

import Monocle.Common as Monocle
import Monocle.Optional exposing (Optional)


-- * evaluation contexts
type alias Context 
    = Optional Expr Expr


-- empty context
hole : Context
hole = { getOption = \expr -> Just expr
       , set = \new _ -> new
       }

-- contexts for list construtor
cons0 : Context
cons0 =
    { getOption= \e -> case e of
                           Cons e0 _ -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        Cons _ e1 -> Cons e0 e1
                        _ -> e
    }
    
cons1 : Context
cons1 =
    { getOption= \e -> case e of
                           Cons _ e1 -> Just e1
                           _ -> Nothing
    , set= \e1 e -> case e of
                        Cons e0 _ -> Cons e0 e1
                        _ -> e
    }


enumFrom0 : Context
enumFrom0 =
    { getOption= \e -> case e of
                           EnumFrom e0 -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        EnumFrom _ -> EnumFrom e0
                        _ -> e
    }

enumFromThen0 : Context
enumFromThen0 =
    { getOption= \e -> case e of
                           EnumFromThen e0 _ -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        EnumFromThen _ e1 -> EnumFromThen e0 e1
                        _ -> e
    }

enumFromThen1 : Context
enumFromThen1 =
    { getOption= \e -> case e of
                           EnumFromThen _ e1 -> Just e1
                           _ -> Nothing
    , set= \e1 e -> case e of
                        EnumFromThen e0 _ -> EnumFromThen e0 e1
                        _ -> e
    }
    
enumFromTo0 : Context
enumFromTo0 =
    { getOption= \e -> case e of
                           EnumFromTo e0 _ -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        EnumFromTo _ e1 -> EnumFromTo e0 e1
                        _ -> e
    }
    
enumFromTo1 : Context
enumFromTo1 =
    { getOption= \e -> case e of
                           EnumFromTo _ e1 -> Just e1
                           _ -> Nothing
    , set= \e1 e -> case e of
                        EnumFromTo e0 _ -> EnumFromTo e0 e1
                        _ -> e
    }

enumFromThenTo0 : Context
enumFromThenTo0 =
    { getOption= \e -> case e of
                           EnumFromThenTo e0 _ _ -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        EnumFromThenTo _ e1 e2 -> EnumFromThenTo e0 e1 e2
                        _ -> e
    }
    
enumFromThenTo1 : Context
enumFromThenTo1 =
    { getOption= \e -> case e of
                           EnumFromThenTo _ e1 _ -> Just e1
                           _ -> Nothing
    , set= \e1 e -> case e of
                        EnumFromThenTo e0 _ e2 -> EnumFromThenTo e0 e1 e2
                        _ -> e
    }
    
enumFromThenTo2 : Context
enumFromThenTo2 =
    { getOption= \e -> case e of
                           EnumFromThenTo _ _ e2 -> Just e2
                           _ -> Nothing
    , set= \e2 e -> case e of
                        EnumFromThenTo e0 e1 _ -> EnumFromThenTo e0 e1 e2
                        _ -> e
    }

    
-- contexts for looking into infix operators
infixOp0 : Context
infixOp0 =
    { getOption= \e -> case e of
                           InfixOp _ e0 _ -> Just e0
                           _ -> Nothing
    , set= \e0 e -> case e of
                        InfixOp op _ e1 -> InfixOp op e0 e1
                        _ -> e
    }

infixOp1 : Context
infixOp1 =
    { getOption = \e -> case e of
                            InfixOp _ _ e1 -> Just e1
                            _ -> Nothing
    , set = \e1 e -> case e of
                         InfixOp op e0 _ -> InfixOp op e0 e1
                         _ -> e
    }
            
-- contexts for looking into applications
app0 : Context
app0 =
    { getOption = \e -> case e of
                            App e0 _ -> Just e0
                            _ -> Nothing
    , set = \e0 e -> case e of
                         App _ args -> App e0 args
                         _ -> e
    }


appArg : Int -> Context
appArg i =
    { getOption = \e -> case e of
                            App _ args -> .getOption (Monocle.list i) args
                            _ -> Nothing
    , set = \n e -> case e of
                        App e0 args -> App e0 (.set (Monocle.list i) n args)
                        _ -> e
    }
    

-- contexts for literal lists and tuples    
listItem : Int -> Context
listItem i =
    { getOption = \e -> case e of
                            ListLit items -> .getOption (Monocle.list i) items
                            _ -> Nothing
    , set = \n e -> case e of
                        ListLit items -> ListLit (.set (Monocle.list i) n items)
                        _ -> e
    }

tupleItem : Int -> Context
tupleItem i =
    { getOption = \e -> case e of
                            TupleLit items -> .getOption (Monocle.list i) items
                            _ -> Nothing
    , set = \n e -> case e of
                        TupleLit items -> TupleLit (.set (Monocle.list i) n items)
                        _ -> e
    }

        
    
--- contexts for if expressions

if0 : Context
if0 =
    { getOption = \e -> case e of
                            IfThenElse e0 e1 e2 -> Just e0
                            _ -> Nothing
    , set = \e0 e -> case e of
                        IfThenElse _ e1 e2 -> IfThenElse e0 e1 e2
                        _ -> e
    }

if1 : Context
if1 =
    { getOption = \e -> case e of
                            IfThenElse e0 e1 e2 -> Just e1
                            _ -> Nothing
    , set = \e1 e -> case e of
                        IfThenElse e0 _ e2 -> IfThenElse e0 e1 e2
                        _ -> e
    }
    
if2 : Context
if2 =
    { getOption = \e -> case e of
                            IfThenElse e0 e1 e2 -> Just e2
                            _ -> Nothing
    , set = \e2 e -> case e of
                        IfThenElse e0 e1 _ -> IfThenElse e0 e1 e2
                        _ -> e
    }
