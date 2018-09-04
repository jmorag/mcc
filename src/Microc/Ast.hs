module Microc.Ast where
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data Op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or deriving (Show, Eq)

data Uop = Neg | Not deriving (Show, Eq)

data Type = TyInt | TyBool | TyFloat | TyVoid deriving (Show, Eq)
data Bind = Bind Type Text deriving (Show, Eq)

data Expr = 
    Literal Int
  | Fliteral Double
  | BoolLit Bool
  | Id Text
  | Binop Op Expr Expr
  | Unop Uop Expr
  | Assign Text Expr
  | Call Text [Expr]
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
  { typ  :: Type
  , name :: Text
  , formals :: [Bind]
  , locals :: [Bind]
  , body :: [Statement]
  }
  deriving (Show, Eq)

data Program = Program [Bind] [Function]

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty Op where
  pretty = \case
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Div -> "/"
    Equal -> "=="
    Neq -> "!="
    Less -> "<"
    Leq -> "<="
    Greater -> ">"
    Geq -> ">="
    And -> "&&"
    Or -> "||"
    
instance Pretty Uop where
  pretty = \case
    Neg -> "-"
    Not -> "!"

instance Pretty Type where
  pretty = \case
    TyInt -> "int"
    TyBool -> "bool"
    TyFloat -> "float"
    TyVoid -> "void"

instance Pretty Bind where
  pretty (Bind ty nm) = pretty ty <+> pretty nm <> semi
