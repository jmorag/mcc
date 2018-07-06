module Semant where

import Ast
import Sast
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

type Vars = M.Map String Type
type Funcs = M.Map String Function
type Env = (Vars, Funcs)

-- Type of semantic checker computations that can only read from the environment
type SemantR = ExceptT String (Reader Env)
-- Type of semantic checker computations that can only write to the environment
type SemantW = ExceptT String (Writer Env)
-- Type of semanctic checker computations that have unrestricted env access
type SemantS = ExceptT String (State Env)

guardInfo :: MonadError e m => Bool -> e -> m a -> m a
guardInfo cond msg rest = if not cond then throwError msg else rest

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

checkProgram' :: Program -> Either String SProgram
checkProgram' (binds, funcs) = do
  return ([], [])


checkExpr :: Expr -> SemantR SExpr
checkExpr expr = let isNumeric t = t `elem` [TyInt, TyFloat] in case expr of
  Literal i  -> return (TyInt, SLiteral i)
  Fliteral f -> return (TyFloat, SFliteral f)
  BoolLit b  -> return (TyBool, SBoolLit b)
  Noexpr     -> return (TyVoid, SNoexpr)

  Id s -> do
    (vars, _) <- ask
    case M.lookup s vars of
      Just ty -> return (ty, SId s)
      Nothing -> throwError $ "Unbound variable " ++ s

  Binop op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    guardInfo (t1 == t2) "incompatible types in binary operation" $ do

    let checkArith = guardInfo (isNumeric t1)
                     "incompatible types in arithmetic operation" $
                     return (t1, SBinop op lhs' rhs')

        checkBool  = guardInfo (t1 == TyBool)
                     "incompatible types in boolean operation" $
                     return (t1, SBinop op lhs' rhs')
    case op of 
      Add -> checkArith; Sub -> checkArith; Mult -> checkArith; Div -> checkArith;
      And -> checkBool; Or -> checkBool;
      -- remaining are relational operators
      _ -> guardInfo (isNumeric t1) 
           "incompatible types in relational operation" $
           return (TyBool, SBinop op lhs' rhs')

  Unop op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> guardInfo (isNumeric ty) "Negative bools are nonsense" $
             return (ty, SUnop Neg e')
      Not -> guardInfo (ty == TyBool) "Boolean negation needs booleans" $
             return (ty, SUnop Not e')

  Assign s e -> do
    e'@(ty, _) <- checkExpr e
    (vars, _) <- ask
    case M.lookup s vars of
      Nothing -> throwError $ "Unbound variable " ++ s
      Just ty' -> guardInfo (ty == ty') 
                  "Attempt to assign expression to var of incompatible type" $
                  return (ty, SAssign s e')

  Call s es -> do
    (_, funcs) <- ask
    case M.lookup s funcs of
      Nothing -> throwError $ "Undefined function " ++ s
      Just f -> do
        es' <- mapM checkExpr es
        guardInfo (map fst es' == map fst (formals f)) 
                  ("Argument of wrong type in call of " ++ name f) $
                  return (typ f, SCall s es')
        

             


