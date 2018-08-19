module Microc.Sast where

import           Microc.Ast
import           Data.Text (Text)

type SExpr = (Type, SExpr')
data SExpr' = 
    SLiteral Int
  | SFliteral Double
  | SBoolLit Bool
  | SId Text
  | SBinop Op SExpr SExpr
  | SUnop Uop SExpr
  | SAssign Text SExpr
  | SCall Text [SExpr]
  | SNoexpr
  deriving (Show, Eq)

data SStatement =
    SExpr SExpr
  | SBlock [SStatement] 
  | SReturn SExpr
  | SIf SExpr SStatement SStatement
  | SFor SExpr SExpr SExpr SStatement
  | SWhile SExpr SStatement
  deriving (Show, Eq)

data SFunction = SFunction
  { styp  :: Type
  , sname :: Text
  , sformals :: [Bind]
  , slocals :: [Bind]
  , sbody :: [SStatement]
  }
  deriving (Show, Eq)

type SProgram = ([Bind], [SFunction])
