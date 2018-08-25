module Microc.Semant (checkProgram) where

import Microc.Ast
import Microc.Sast
import qualified Data.Map as M
import Data.Tuple (swap)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)

type Vars = M.Map Text Type
type Funcs = M.Map Text Function
data Env = Env { vars :: Vars, funcs :: Funcs, thisFunc :: Function }

type SemantS = ExceptT Text (State Env)

guardInfo :: MonadError e m => Bool -> e -> m a -> m a
guardInfo cond msg rest = if not cond then throwError msg else rest

checkBinds :: Text -> [Bind] -> SemantS [Bind]
checkBinds kind = mapM $ checkBind kind
  where
    checkBind kind (TyVoid, name) =
      throwError $ "illegal void binding in " <> kind <> " variable " <> name

    checkBind kind (ty, name) = do
      vars <- gets vars
      guardInfo (M.notMember name vars)
                ("Illegal duplicate " <> kind <> " binding " <> name)
        $ do
            modify $ \env -> env { vars = M.insert name ty vars }
            return (ty, name)

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
    vars <- gets vars
    case M.lookup s vars of
      Just ty -> return (ty, SId s)
      Nothing -> throwError $ "Unbound variable " <> s

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
    vars <- gets vars
    case M.lookup s vars of
      Nothing -> throwError $ "Unbound variable " <> s
      Just ty' -> guardInfo (ty == ty') 
                  "Attempt to assign expression to var of incompatible type" $
                  return (ty, SAssign s e')

  Call s es -> do
    funcs <- gets funcs
    case M.lookup s funcs of
      Nothing -> throwError $ "Undefined function " <> s
      Just f -> do
        es' <- mapM checkExpr es
        guardInfo (map fst es' == map fst (formals f)) 
                  ("Argument of wrong type in call of " <> name f) $
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
    fun <- gets thisFunc
    guardInfo (ty == typ fun) 
      "Type of return expression inconsistent with declared type" $
      return $ SReturn e

  Block sl -> 
    let flattened = flatten sl
    in guardInfo (nothingFollowsRet flattened)
       ("Nothing can follow a return: error in " <> T.pack (show stmt)) $
       SBlock <$> mapM checkStatement sl
    where
      flatten [] = []
      flatten (Block s:ss) = flatten (s ++ ss)
      flatten (s:ss) = s : flatten ss

      nothingFollowsRet [] = True
      nothingFollowsRet [Return _] = True
      nothingFollowsRet (s:ss) = 
        case s of Return _ -> False; _ -> nothingFollowsRet ss

checkFunction :: Function -> SemantS SFunction    
checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- gets funcs
  guardInfo (isNothing $ M.lookup (name func) funcs) 
            ("Redeclaration of function " <> name func) $
    -- add this func to symbol table
    modify $ \env -> 
      env { funcs = M.insert (name func) func funcs, thisFunc = func }

  -- check variables
  formals' <- checkBinds "formal" (formals func)
  locals'  <- checkBinds "local"  (locals func)
  -- create local variable table
  globals <- gets vars
  let allVars = M.toList globals ++ map swap (formals' ++ locals')
      localVars = M.fromList allVars

  -- Overwrite local variables into the environment for the body checking
  modify $ \env -> env { vars = localVars }
  body' <- checkStatement (Block $ body func)
  -- Set the env back to the way it was
  modify $ \env -> env { vars = globals }

  case body' of 
    SBlock body'' -> return $ SFunction { styp = typ func 
                                        , sname = name func
                                        , sformals = formals'
                                        , slocals = locals'
                                        , sbody = body''
                                        }
    _ -> error "Internal error - block didn't become a block?"


checkProgram :: Program -> Either Text SProgram
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
    globals <- checkBinds "global" binds
    modify $ \env -> env { vars = M.fromList (map swap globals) }
    funcs' <- mapM checkFunction funcs
    case find (\f -> sname f == "main") funcs' of
      Nothing -> throwError "Error, main function not defined"
      Just _ -> return (globals, funcs')
