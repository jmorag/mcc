module Ast where

data Op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or deriving (Show, Eq)

data Uop = Neg | Not deriving (Show, Eq)

data Type = TyInt | TyBool | TyFloat | TyVoid deriving (Show, Eq)
type Bind = (Type, String)

data Expr = 
    Literal Int
  | Fliteral Double
  | BoolLit Bool
  | Id String
  | Binop Op Expr Expr
  | Unop Uop Expr
  | Assign String Expr
  | Call String [Expr]
  | Noexpr
  deriving (Show, Eq)


data Statement = 
    Expr Expr
  | Block [Statement]
  | Return Expr
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  deriving (Show, Eq)


data Function = Function
  { name :: String
  , typ  :: Type
  , formals :: [Bind]
  , locals :: [Bind]
  , body :: [Statement]
  }
  deriving (Show, Eq)

type Program = ([Bind], [Function])
