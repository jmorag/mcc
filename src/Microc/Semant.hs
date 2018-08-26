{-# LANGUAGE LambdaCase #-}
module Microc.Semant (checkProgram) where

import Microc.Ast
import Microc.Sast
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)

type Vars = M.Map (Text, VarKind) Type
type Funcs = M.Map Text Function
data VarKind = Global | Formal | Local deriving (Show, Eq, Ord)

data Env = Env { vars     :: Vars
               , funcs    :: Funcs
               , thisFunc :: Function }

type SemantS = ExceptT Text (State Env)

tshow :: Show a => a -> Text
tshow = T.pack . show

guardInfo :: MonadError e m => Bool -> e -> m a -> m a
guardInfo cond msg rest = if not cond then throwError msg else rest

checkBinds :: VarKind -> [Bind] -> SemantS [Bind]
checkBinds kind = mapM $ \case
    (TyVoid, name) -> throwError $ 
      T.unwords ["illegal void binding in", tshow kind, "variable", name]
    (ty, name) -> do
      vars <- gets vars
      guardInfo (M.notMember (name, kind) vars)
                (T.unwords ["Illegal duplicate", tshow kind, "binding", name]) $ do
                modify $ \env -> env { vars = M.insert (name, kind) ty vars } 
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
    let foundVars = map (\kind -> M.lookup (s, kind) vars) [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ "Unbound variable " <> s
      Just ty -> return (ty, SId s)

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
    rhs@(ty, _) <- checkExpr e
    (ty', _) <- checkExpr (Id s)
    guardInfo (ty == ty') 
     "Attempt to assign expression to var of incompatible type" $
     return (ty, SAssign s rhs)
    
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
    guardInfo (ty == TyBool) "Expected boolean expression" $
      SWhile cond' <$> checkStatement action

  Return expr -> do
    e@(ty, _) <- checkExpr expr
    fun <- gets thisFunc
    guardInfo (ty == typ fun) 
      "Type of return expression inconsistent with declared type" $
      return $ SReturn e

  Block sl -> 
    let flattened = flatten sl
    in guardInfo (nothingFollowsRet flattened)
       ("Nothing can follow a return: error in " <> tshow stmt) $
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
  guardInfo (M.notMember (name func) funcs) 
            ("Redeclaration of function " <> name func) $
    -- add this func to symbol table
    modify $ \env -> 
      env { funcs = M.insert (name func) func funcs, thisFunc = func }

  -- Save the symbol table prior to adding formal and local variables to it
  oldState <- get

  -- check variables
  formals' <- checkBinds Formal (formals func)
  locals'  <- checkBinds Local  (locals func)

  -- Check the body of the function with all the local variables added to the
  -- symbol table
  body' <- checkStatement (Block $ body func)

  -- remove all local variables from symbol table
  put oldState

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
  baseEnv = Env { vars = M.empty
                , funcs = builtIns
                , thisFunc = garbageFunc }
  garbageFunc = Function { typ = TyVoid
                         , name = ""
                         , formals = []
                         , locals = []
                         , body = []
                         }
  checkProgram' (binds, funcs) = do
    globals <- checkBinds Global binds
    funcs' <- mapM checkFunction funcs
    case find (\f -> sname f == "main") funcs' of
      Nothing -> throwError "Error, main function not defined"
      Just _ -> return (globals, funcs')
