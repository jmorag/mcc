module Semant where

import Ast
import Sast
import qualified Data.Map as M
import Control.Monad.State

type Vars = M.Map String Type
type Funcs = M.Map String Function

checkBinds :: String -> [Bind] -> [Bind]
checkBinds kind binds = go M.empty binds
  where
    go checked [] = map (\(name, typ) -> (typ, name)) (M.toList checked)
    go checked (b:bs) = case b of 
      (TyVoid, _) -> error $ "Illegal void binding " ++ kind ++ " " ++ snd b
      _ -> case M.lookup (snd b) checked of
        Nothing -> go (M.insert (snd b) (fst b) checked) bs
        Just _ -> error $ "Illegal duplicate binding " ++ kind ++ " " ++ snd b

builtIns :: Funcs
builtIns = M.fromList $ map toFunc
  [("print", TyInt), ("printb", TyBool), ("printf", TyFloat), ("printbig", TyInt)]
  where
    toFunc (name, ty) = (name, Function TyVoid name [(ty, "x")] [] [])

checkProgram :: Program -> SProgram
checkProgram (binds, funcs) = (checkBinds "global" binds, map checkFunc funcs)

checkFunc :: Function -> SFunction
checkFunc func = SFunction { styp = typ func 
                           , sname = name func
                           , sformals = checkBinds "formal" (formals func)
                           , slocals = checkBinds "local" (locals func)
                           , sbody = map checkStatement (body func)
                           }

checkStatement :: Statement -> SStatement
checkStatement = undefined
