{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Microc.Semant (checkProgram) where

import Microc.Ast
import Microc.Sast
import Microc.Semant.Error
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.List (find)

type Vars = M.Map (Text, VarKind) Type
type Funcs = M.Map Text Function

data Env = Env { vars     :: Vars
               , funcs    :: Funcs
               , thisFunc :: Function }

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> [Bind] -> Semant [Bind]
checkBinds kind binds = do
  currentFunc <- if kind == Global then return Nothing else Just <$> gets thisFunc
  forM binds $ \case
    Bind TyVoid name ->
      throwError $ IllegalBinding name Void kind currentFunc

    Bind ty name -> do
      vars <- gets vars
      unless (M.notMember (name, kind) vars) $
        throwError $ IllegalBinding name Duplicate kind currentFunc
      modify $ \env -> env { vars = M.insert (name, kind) ty vars } 
      return $ Bind ty name

builtIns :: Funcs
builtIns = M.fromList $ map toFunc
  [("print", TyInt), ("printb", TyBool), ("printf", TyFloat), ("printbig", TyInt)]
  where
    toFunc (name, ty) = (name, Function TyVoid name [Bind ty "x"] [] [])

checkExpr :: Expr -> Semant SExpr
checkExpr expr = let isNumeric t = t `elem` [TyInt, TyFloat] in case expr of
  Literal i  -> return (TyInt, SLiteral i)
  Fliteral f -> return (TyFloat, SFliteral f)
  BoolLit b  -> return (TyBool, SBoolLit b)
  Noexpr     -> return (TyVoid, SNoexpr)

  Id s -> do
    vars <- gets vars
    let foundVars = map (\kind -> M.lookup (s, kind) vars) [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol s Var expr
      Just ty -> return (ty, SId s)

  Binop op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)

    let checkArith = unless (isNumeric t1) 
          (throwError $ TypeError [TyInt, TyFloat] t1 (Expr expr)) >> 
          return (t1, SBinop op lhs' rhs')

        checkBool  = unless (t1 == TyBool) 
          (throwError $ TypeError [TyBool] t1 (Expr expr)) >> 
          return (t1, SBinop op lhs' rhs')

        checkFloat = unless (t1 == TyFloat) 
          (throwError $ TypeError [TyFloat] t1 (Expr expr)) >> 
          return (t1, SBinop op lhs' rhs')

    case op of 
      Add -> checkArith; Sub -> checkArith; Mult -> checkArith; Div -> checkArith;
      Power -> checkFloat;
      And -> checkBool; Or -> checkBool;
      -- remaining are relational operators
      _ -> do unless (isNumeric t1) $
                throwError $ TypeError [TyInt, TyFloat] t1 (Expr expr)
              return (TyBool, SBinop op lhs' rhs')

  Unop op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> do unless (isNumeric ty) $ throwError $ TypeError [TyInt, TyFloat] ty (Expr expr)
                return (ty, SUnop Neg e')
      Not -> do unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty (Expr expr)
                return (ty, SUnop Not e')

  Assign s e -> do
    rhs@(ty, _) <- checkExpr e
    (ty', _) <- checkExpr (Id s)
    unless (ty == ty') $ throwError $ TypeError [ty'] ty (Expr expr)
    return (ty, SAssign s rhs)
    
  Call s es -> do
    funcs <- gets funcs
    case M.lookup s funcs of
      Nothing -> throwError $ UndefinedSymbol s Func expr
      Just f -> do
        es' <- mapM checkExpr es
        -- Check that the correct number of arguments was provided
        let nFormals = length (formals f)
            nActuals = length es
        unless (nFormals == nActuals) $ throwError $ ArgError nFormals nActuals expr
        -- Check that types of arguments match
        forM_ (zip (map fst es') (map (\(Bind ty _) -> ty) (formals f))) $ \(callSite, defSite) ->
          unless (callSite == defSite) $
            throwError $ TypeError { expected = [defSite], got = callSite, errorLoc = Expr expr }
        return (typ f, SCall s es')
        
checkStatement :: Statement -> Semant SStatement
checkStatement stmt = case stmt of
  Expr e -> SExpr <$> checkExpr e

  If pred cons alt -> do
    pred'@(ty, _) <- checkExpr pred
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    SIf pred' <$> checkStatement cons <*> checkStatement alt
    
  For init cond inc action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    init' <- checkExpr init
    inc'  <- checkExpr inc
    action' <- checkStatement action
    return $ SFor init' cond' inc' action'

  While cond action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    SWhile cond' <$> checkStatement action

  Return expr -> do
    e@(ty, _) <- checkExpr expr
    fun <- gets thisFunc
    unless (ty == typ fun) $ 
      throwError $ TypeError [typ fun] ty stmt
    return $ SReturn e

  Block sl -> do
    let flattened = flatten sl
    unless (nothingFollowsRet flattened) $ throwError (DeadCode stmt)
    SBlock <$> mapM checkStatement sl
    where
      flatten [] = []
      flatten (Block s:ss) = flatten (s ++ ss)
      flatten (s:ss) = s : flatten ss

      nothingFollowsRet [] = True
      nothingFollowsRet [Return _] = True
      nothingFollowsRet (s:ss) = 
        case s of Return _ -> False; _ -> nothingFollowsRet ss

checkFunction :: Function -> Semant SFunction    
checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- gets funcs
  unless (M.notMember (name func) funcs) $
    throwError $ Redeclaration (name func)
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

checkProgram :: Program -> Either SemantError SProgram
checkProgram (Program binds funcs) = 
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
      Nothing -> throwError NoMain
      Just _ -> return (globals, funcs')
