module Ast where


import Data.List (intercalate)


type Name = String


data Def = Def Name Type Expr
  deriving (Eq)


data Type
  = TInt
  | TBool
  | TArrow Type Type
  deriving (Eq)


data Expr
  = Apply Expr Expr
  | Bool Bool
  | Equal Expr Expr
  | Lambda [(Name, Type)] Type Expr
  | If Expr Expr Expr
  | Int Int
  | Less Expr Expr
  | Minus Expr Expr
  | Plus Expr Expr
  | Times Expr Expr
  | Var Name
  deriving (Eq)


--------------------------------------------------------------------------------
-- Show: "pretty" printing


instance Show Def where
  show (Def name ty expr) =
    "let " ++ name ++ ":" ++ show ty ++ " = " ++ show expr


instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2


instance Show Expr where
  show expr =
    let
      showName (n, ty) = n ++ ":" ++ show ty
      showNamelist = intercalate ", " . map showName
    in
    case expr of
      Apply e1 e2 -> show e1 ++ " (" ++ show e2 ++ ")"
      Bool b -> if b then "true" else "false"
      Equal e1 e2 -> show e1 ++ " = " ++ show e2
      Lambda names ty2 e1 ->
        "fun(" ++ showNamelist names ++ "):" ++ show ty2 ++ " {" ++ show e1 ++ " }"
      If e1 e2 e3 ->
        "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
      Int i -> show i
      Less e1 e2 -> show e1 ++ " < " ++ show e2
      Minus e1 e2 -> show e1 ++ " - " ++ show e2
      Plus e1 e2 -> show e1 ++ " + " ++ show e2
      Times e1 e2  -> show e1 ++ " * " ++ show e2
      Var name -> name
