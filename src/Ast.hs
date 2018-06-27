module Ast (
) where

import Data.Functor
import Data.Functor.Foldable 


data Op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

data Uop = Neg | Not

data Typ = Int | Bool | Float | Void

data ExprF a = 
    Literal Int
  | Fliteral String
  | BoolLit Bool
  | Id String
  | Binop Op a a
  | Unop Uop a
  | Assign String a
  | Call String [a]
  | Noexpr

