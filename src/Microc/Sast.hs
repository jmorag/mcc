 {-# LANGUAGE StrictData #-}
module Microc.Sast
  ( SExpr
  , SExpr'(..)
  , LValue(..)
  , SStatement(..)
  , SFunction(..)
  , SProgram
  ) where

import           Microc.Ast
import           Data.Text                      ( Text )

type SExpr = (Type, SExpr')
data SExpr' =
    SLiteral Int
  | SFliteral Double
  | SStrLit Text
  | SCharLit Int
  | SBoolLit Bool
  | SNull
  | SBinop Op SExpr SExpr
  | SUnop Uop SExpr
  | SCall Text [SExpr]
  | SCast Type SExpr
  | LVal LValue
  | SAssign LValue SExpr
  | SAddr LValue
  | SSizeof Type
  | SNoexpr
  deriving stock (Show, Eq)

-- | LValues are the class of assignable expressions that can appear
-- on the Left side on the '=' operator and that can have their addresses
-- taken.
data LValue = SDeref SExpr | SAccess LValue Int | SId Text
  deriving stock (Show, Eq)

data SStatement =
    SExpr SExpr
  | SBlock [SStatement]
  | SReturn SExpr
  | SIf SExpr SStatement SStatement
  | SDoWhile SExpr SStatement
  deriving stock (Show, Eq)

data SFunction = SFunction
  { styp  :: Type
  , sname :: Text
  , sformals :: [Bind]
  , slocals :: [Bind]
  , sbody :: SStatement
  }
  deriving stock (Show, Eq)

type SProgram = ([Struct], [Bind], [SFunction])
