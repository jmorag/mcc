module Microc.Semant.Analysis where

import Microc.Sast

-- | True if statement is a return
data CFG = Empty | Seq Bool CFG | Branch Bool CFG CFG

-- | By this point, the dead code invariant will have been checked
genCFG :: [SStatement] -> CFG
genCFG [] = Empty
genCFG (s:ss) = case s of
    SReturn _ -> Seq True (genCFG ss)
    SIf _ cons alt -> Branch False (genCFG (cons : ss)) (genCFG (alt:ss))
    SWhile _ stmt -> Seq False (genCFG (stmt:ss))
    SFor  _ _ _ stmt -> Seq False (genCFG (stmt:ss))
    SBlock stmts -> genCFG (stmts <> ss)
    _ -> Seq False (genCFG ss)

-- | Traverses cfg and returns true if all leaves are true
validate :: CFG -> Bool
validate = \case
  Empty -> False
  Seq b Empty -> b
  Seq _ rest -> validate rest
  Branch False left right -> validate left && validate right
  Branch True _ _ -> undefined
