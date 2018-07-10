module Semant where

import Ast
import Sast
import qualified Data.Map as M
import Data.Tuple (swap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

type Vars = M.Map String Type
type Funcs = M.Map String Function
data Env = Env { vars :: Vars, funcs :: Funcs, thisFunc :: Function }

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

checkExpr :: Expr -> SemantS SExpr
checkExpr expr = let isNumeric t = t `elem` [TyInt, TyFloat] in case expr of
  Literal i  -> return (TyInt, SLiteral i)
  Fliteral f -> return (TyFloat, SFliteral f)
  BoolLit b  -> return (TyBool, SBoolLit b)
  Noexpr     -> return (TyVoid, SNoexpr)

  Id s -> do
    vars <- vars <$> get
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
                     "expected boolean expression" $
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
    vars <- vars <$> get
    case M.lookup s vars of
      Nothing -> throwError $ "Unbound variable " ++ s
      Just ty' -> guardInfo (ty == ty') 
                  "Attempt to assign expression to var of incompatible type" $
                  return (ty, SAssign s e')

  Call s es -> do
    funcs <- funcs <$> get
    case M.lookup s funcs of
      Nothing -> throwError $ "Undefined function " ++ s
      Just f -> do
        es' <- mapM checkExpr es
        guardInfo (map fst es' == map fst (formals f)) 
                  ("Argument of wrong type in call of " ++ name f) $
                  return (typ f, SCall s es')
        
checkStatement :: Statement -> SemantS SStatement
checkStatement stmt = case stmt of
  Expr e -> SExpr <$> checkExpr e

  If pred cons alt -> do
    pred'@(ty, _) <- checkExpr pred
    guardInfo (ty == TyBool) "Expected boolean expression" $ do
    cons' <- checkStatement cons
    alt'  <- checkStatement alt
    return $ SIf pred' cons' alt'
    
  For init cond inc action -> do
    cond'@(ty, _) <- checkExpr cond
    guardInfo (ty == TyBool) "Expected boolean expression" $ do
    init' <- checkExpr init
    inc'  <- checkExpr inc
    action' <- checkStatement action
    return $ SFor init' cond' inc' action'

  While cond action -> do
    cond'@(ty, _) <- checkExpr cond
    guardInfo (ty == TyBool) "Expected boolean expression" $ do
    action' <- checkStatement action
    return $ SWhile cond' action'

  Return expr -> do
    e@(ty, _) <- checkExpr expr
    fun <- thisFunc <$> get
    guardInfo (ty == typ fun) 
      "Type of return expression inconsistent with declared type" $ do
    return $ SReturn e

  Block sl -> case sl of
    -- unsure is this first case is necessary...
    [s@(Return _)] -> do s' <- checkStatement s; return $ SBlock [s']
    -------------------------
    (Return _) : _ -> throwError "nothing can follow a return"
    Block sl : ss -> checkStatement $ Block (sl ++ ss)
    _ -> SBlock <$> mapM checkStatement sl

    
checkFunction :: Function -> SemantS SFunction    
checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- funcs <$> get
  guardInfo (M.lookup (name func) funcs == Nothing) 
            ("Redeclaration of function " ++ name func) $ do
  -- add this func to symbol table
  modify $ \env -> 
    env { funcs = M.insert (name func) func funcs, thisFunc = func }

  -- check variables
  let formals' = checkBinds "formal" (formals func)
  let locals'  = checkBinds "local"  (locals func)
  -- create local variable table
  globals <- vars <$> get
  let allVars = M.toList globals ++ (map swap (formals' ++ locals'))
      localVars = M.fromList allVars

  -- Overwrite local variables into the environment for the body checking
  modify $ \env -> env { vars = localVars }
  body' <- mapM checkStatement (body func)
  -- Set the env back to the way it was
  modify $ \env -> env { vars = globals }

  return $ SFunction { styp = typ func 
                     , sname = name func
                     , sformals = formals'
                     , slocals = locals'
                     , sbody = body'
                     }


checkProgram :: Program -> Either String SProgram
checkProgram (binds, funcs) = 
  evalState (runExceptT (checkProgram' (binds, funcs))) baseEnv
  where
  baseEnv = Env { vars = M.empty, 
                  funcs = builtIns, 
                  thisFunc = garbageFunc }
  garbageFunc = Function { typ = TyVoid
                         , name = ""
                         , formals = []
                         , locals = []
                         , body = []
                         }
  checkProgram' (binds, funcs) = do
    let globals = checkBinds "global" binds
    modify $ \env -> env { vars = M.fromList (map swap globals) }
    funcs' <- mapM checkFunction funcs
    return (globals, funcs')
  
