{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Microc.Semant.Error (
) where

import Microc.Ast
import Data.Text (Text)

data ErrorType = IllegalBinding BindingKind VarKind
               | UndefinedSymbol SymbolKind
               | TypeError { expected :: Type, got :: Type }
               | Other Text

data BindingKind = Duplicate | Void
data SymbolKind = Variable | Function

data SemantError = SemantError { type' :: ErrorType
                               , statement :: Statement
                               , function :: Function }
