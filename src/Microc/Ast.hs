 {-# LANGUAGE StrictData #-}
module Microc.Ast
  ( Op(..)
  , Uop(..)
  , Struct(..)
  , Type(..)
  , Bind(..)
  , Expr(..)
  , Statement(..)
  , Function(..)
  , Program(..)
  ) where

import           Data.Char                      ( chr )
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc

data Op = Add
        | Sub
        | Mult
        | Div
        | Power
        | Equal
        | Neq
        | Less
        | Leq
        | Greater
        | Geq
        | And
        | Or
        | BitAnd
        | BitOr
        deriving stock (Show, Eq)

data Uop = Neg
         | Not
         deriving stock (Show, Eq)

data Struct = Struct
  { structName   :: Text
  , structFields :: [Bind]
  }
  deriving stock (Show, Eq)

data Type = Pointer Type
          | TyInt
          | TyBool
          | TyFloat
          | TyChar
          | TyVoid
          | TyStruct Text
          deriving stock (Show, Eq)
data Bind = Bind
  { bindType :: Type
  , bindName :: Text
  }
  deriving stock (Show, Eq)

data Expr = Literal Int
          | StrLit Text
          | CharLit Int -- ^ Chars get lowered to ints during codegen
          | Fliteral Double
          | BoolLit Bool
          | Null
          | Id Text
          | Binop Op Expr Expr
          | Unop Uop Expr
          | Call Text [Expr]
          | Cast Type Expr
          | Access Expr Expr
          | Deref Expr
          | Addr Expr
          | Assign Expr Expr
          | Sizeof Type
          | Noexpr
          deriving stock (Show, Eq)

data Statement = Expr Expr
               | Block [Statement]
               | Return Expr
               | If Expr Statement Statement
               | For Expr Expr Expr Statement
               | While Expr Statement
               deriving stock (Show, Eq)

data Function = Function
  { typ     :: Type
  , name    :: Text
  , formals :: [Bind]
  , locals  :: [Bind]
  , body    :: [Statement]
  }
  deriving stock (Show, Eq)

data Program = Program [Struct] [Bind] [Function]
  deriving stock (Eq, Show)

--------------------------------------------
-- Pretty instances
--------------------------------------------
instance Pretty Op where
  pretty = \case
    Add     -> "+"
    Sub     -> "-"
    Mult    -> "*"
    Div     -> "/"
    Power   -> "**"
    Equal   -> "=="
    Neq     -> "!="
    Less    -> "<"
    Leq     -> "<="
    Greater -> ">"
    Geq     -> ">="
    And     -> "&&"
    Or      -> "||"
    BitAnd  -> "&"
    BitOr   -> "|"

instance Pretty Uop where
  pretty = \case
    Neg -> "-"
    Not -> "!"

instance Pretty Struct where
  pretty (Struct nm binds) =
    "struct"
      <+> pretty nm
      <+> lbrace
      <>  hardline
      <>  indent 4 (vsep (map (\b -> pretty b <> ";") binds))
      <>  hardline
      <>  rbrace
      <>  ";"

instance Pretty Type where
  pretty = \case
    TyInt      -> "int"
    TyBool     -> "bool"
    TyChar     -> "char"
    TyFloat    -> "float"
    TyVoid     -> "void"
    Pointer  t -> pretty t <+> "*"
    TyStruct n -> "struct" <+> pretty n

instance Pretty Bind where
  pretty (Bind ty nm) = pretty ty <+> pretty nm

instance Pretty Expr where
  pretty = \case
    Literal  i          -> pretty i
    Fliteral f          -> pretty f
    CharLit  c          -> squotes $ pretty (chr c)
    StrLit   s          -> dquotes $ pretty s
    BoolLit  b          -> if b then "true" else "false"
    Null                -> "NULL"
    Id t                -> pretty t
    Binop op lhs rhs    -> hsep [pretty lhs, pretty op, pretty rhs]
    Unop   op     e     -> pretty op <> parens (pretty e)
    Call   f      es    -> pretty f <> tupled (map pretty es)
    Cast   t      e     -> parens (pretty t) <> parens (pretty e)
    Access struct field -> pretty struct <> "." <> pretty field
    Assign lhs    rhs   -> pretty lhs <+> "=" <+> pretty rhs
    Deref  e            -> "*" <> parens (pretty e)
    Addr   e            -> "&" <> parens (pretty e)
    Sizeof t            -> "sizeof" <> parens (pretty t)
    Noexpr              -> mempty


instance Pretty Statement where
  pretty = \case
    Expr e -> pretty e <> semi
    Block ss ->
      lbrace
        <> hardline
        <> indent 4 (vsep (map pretty ss))
        <> hardline
        <> rbrace
    Return e -> "return" <+> pretty e <> semi
    If pred cons alt ->
      "if" <+> parens (pretty pred) <+> pretty cons <> prettyAlt
     where
      prettyAlt = case alt of
        Block [] -> mempty
        _        -> hardline <> "else" <+> pretty alt
    For init cond inc body ->
      "for"
        <+> encloseSep lparen rparen semi [pretty init, pretty cond, pretty inc]
        <+> pretty body
    While cond body -> "while" <+> parens (pretty cond) <+> pretty body

instance Pretty Function where
  pretty (Function typ name formals locals body) =
    pretty typ
      <+> pretty name
      <>  tupled (map pretty formals)
      <>  hardline
      <>  lbrace
      <>  hardline
      <>  indent 4 (hardsep (map decl locals ++ map pretty body))
      <>  hardline
      <>  rbrace
      <>  hardline

instance Pretty Program where
  pretty (Program structs binds funcs) =
    hardsep (map pretty structs ++ map decl binds ++ map pretty funcs)

decl :: Pretty a => a -> Doc ann
decl bind = pretty bind <> semi

-- | Separates many docs with hardlines
hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)
