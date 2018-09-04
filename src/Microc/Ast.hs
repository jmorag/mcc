{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

data Program = Program [Bind] [Function] deriving (Eq, Show)

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
  pretty (Bind ty nm) = pretty ty <+> pretty nm

instance Pretty Expr where
  pretty = \case
    Literal i -> pretty i
    Fliteral f -> pretty f
    BoolLit b -> if b then "true" else "false"
    Id t -> pretty t
    -- Parenthesize binops so that we can see if precedence worked out
    -- correctly in the pretty printed output
    Binop op lhs rhs -> parens $ hsep [pretty lhs, pretty op, pretty rhs]
    Unop op e -> pretty op <> pretty e
    Assign n e -> pretty n <+> equals <+> pretty e
    Call f es -> pretty f <> tupled (map pretty es)
    Noexpr -> mempty

instance Pretty Statement where
  pretty = \case
    Expr e -> pretty e <> semi
    Block ss -> braces $ sep (map pretty ss)
    Return e -> "return" <+> pretty e <> semi
    -- Fix alignment later
    If pred cons alt -> "if" <> parens (pretty pred) <> hardline <> indent 4 (pretty cons) <> prettyAlt
      where 
        prettyAlt = 
          case alt of 
            Block [] -> mempty
            _ -> hardline <> "else" <> hardline <> indent 4 (pretty alt)
    For init cond inc body -> "for" <> 
      encloseSep lparen rparen semi [pretty init, pretty cond, pretty inc] 
      <> indent 4 (pretty body)
    While cond body -> "while" <> parens (pretty cond) <> hardline <> indent 4 (pretty body)

instance Pretty Function where
  pretty (Function typ name formals locals body) = 
    pretty typ <+> pretty name <> tupled (map pretty formals) <+> lbrace <> hardline <>
    indent 4 (sep (map decl locals ++ map pretty body)) <> line <> rbrace

instance Pretty Program where
  pretty (Program binds funcs) = vsep (map decl binds ++ map pretty funcs)

decl :: Pretty a => a -> Doc ann
decl bind = pretty bind <> semi
