module Microc.Semant.Error where

import Microc.Ast
import Data.Text (Text)

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
