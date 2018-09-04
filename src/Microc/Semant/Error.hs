module Microc.Semant.Error where

import Microc.Ast
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

type Name = Text
data SemantError = IllegalBinding Name BindingKind VarKind (Maybe Function)
                 | UndefinedSymbol Name SymbolKind Expr
                 | TypeError { expected :: [Type], got :: Type, errorLoc :: Statement }
                 | ArgError { nExpected :: Int, nGot :: Int, callSite :: Expr }
                 | Redeclaration Name
                 | NoMain
                 | DeadCode Statement -- ^ For statements in a block following a return
                 deriving (Show)

data BindingKind = Duplicate | Void deriving (Show)
data SymbolKind = Var | Func deriving (Show)

data VarKind = Global | Formal | Local deriving (Show, Eq, Ord)

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
    IllegalBinding nm bindKind varKind func -> 
      "Error: Illegal" <+> pretty bindKind <+> pretty varKind <+> 
      "binding," <+> pretty nm <+> 
      maybe mempty (\f -> "in function" <+> pretty (name f)) func

    UndefinedSymbol nm symKind expr ->
      "Undefined" <+> pretty symKind <+> pretty nm <+> 
      "referenced in:" <> hardline <> pretty expr

    TypeError expected got stmt ->
      "Type error: expected one of" <+> pretty expected <+> "but got" <+> pretty got
      <> ". Error occured in statement:" <> hardline <> pretty stmt

    ArgError nExpected nGot callSite ->
      "Argument error: function expected" <+> pretty nExpected <+>
      "arguments, but was only called with" <+> pretty nGot <+> "arguments"
      <> ". Error occured in call:" <> hardline <> pretty callSite

    Redeclaration name -> "Error: redeclaration of function" <+> pretty name

    NoMain -> "Error: main function not defined"

    DeadCode stmt -> 
      "Error: nothing may follow a return. Error occured in statement:" <>
      hardline <> pretty stmt
