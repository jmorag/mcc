{-# LANGUAGE StrictData #-}
module Microc.Semant.Error
  ( BindingKind (..),
    BindingLoc (..),
    SemantError (..),
    SymbolKind (..),
    VarKind (..),
  )
where

import           Microc.Ast
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc

type Name = Text
data BindingLoc = F Function | S Struct | Toplevel deriving stock Show
data SemantError =
    IllegalBinding Name BindingKind VarKind BindingLoc
  | UndefinedSymbol Name SymbolKind Expr
  | TypeError { expected :: [Type], got :: Type, errorLoc :: Statement }
  | CastError { to :: Type, from :: Type, castLoc :: Statement }
  | ArgError { nExpected :: Int, nGot :: Int, callSite :: Expr }
  | Redeclaration Name
  | NoMain
  | AddressError Expr
  | AssignmentError { lhs :: Expr, rhs :: Expr }
  | AccessError { struct :: Expr, field :: Expr }
  | DeadCode Statement -- ^ For statements in a block following a return
  deriving stock (Show)

data BindingKind = Duplicate | Void deriving stock (Show)
data SymbolKind = Var | Func deriving stock (Show)

data VarKind = Global | Formal | Local | StructField deriving stock (Show, Eq, Ord)

instance Pretty VarKind where
  pretty = unsafeViaShow

instance Pretty SymbolKind where
  pretty = \case
    Var -> "variable"
    Func -> "function"

instance Pretty BindingKind where
  pretty = unsafeViaShow

instance Pretty SemantError where
  pretty = \case
    IllegalBinding nm bindKind varKind loc ->
      "Error: Illegal" <+> pretty bindKind <+> pretty varKind <+>
      "binding," <+> pretty nm <+> case loc of
      F f -> "in function" <+> pretty (name f)
      S (Struct sname _) -> "in struct" <+> pretty sname
      Toplevel -> mempty


    UndefinedSymbol nm symKind expr ->
      "Undefined" <+> pretty symKind <+> pretty nm <+>
      "referenced in:" <> hardline <> pretty expr

    TypeError expected got stmt ->
      "Type error: expected one of" <+> pretty expected <+> "but got"
      <+> pretty got <> ". Error occured in statement:" <> hardline <> pretty stmt
    CastError to from stmt ->
      "Cast error: can only cast between pointers, from ints to floats, or between pointers and ints, not from" <+> pretty from <+> "to" <+> pretty to <> ". Error occured in statement:" <> hardline <> pretty stmt


    ArgError nExpected nGot callSite ->
      "Argument error: function expected" <+> pretty nExpected <+>
      "arguments, but was called with" <+> pretty nGot <+> "arguments"
      <> ". Error occured in call:" <> hardline <> pretty callSite

    Redeclaration name -> "Error: redeclaration of function" <+> pretty name

    NoMain -> "Error: main function not defined"

    AssignmentError lhs rhs ->
      "Cannot assign" <+> pretty rhs <+> "to" <+> pretty lhs

    AddressError e ->
      "Cannot take address of" <> pretty e
      
    AccessError struct field ->
      "Cannot access" <+> pretty struct <+> "with" <+> pretty field

    DeadCode stmt ->
      "Error: nothing may follow a return. Error occured in statement:" <>
      hardline <> pretty stmt
