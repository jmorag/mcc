{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Microc.Semant
  ( checkProgram
  )
where

import           Microc.Ast
import           Microc.Sast
import           Microc.Semant.Error
import           Microc.Semant.Analysis
import           Microc.Utils
import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Data.List                      ( find
                                                , findIndex
                                                )

type Vars = M.Map (Text, VarKind) Type
type Funcs = M.Map Text Function
type Structs = [Struct]
data Env = Env { vars     :: Vars
               , funcs    :: Funcs
               , structs  :: Structs
               , thisFunc :: Function
               }

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> [Bind] -> Semant [Bind]
checkBinds kind binds = do
  currentFunc <- case kind of
    Global      -> pure Toplevel
    StructField -> pure Toplevel -- we don't keep the current struct in semant
    _           -> F <$> gets thisFunc
  forM binds $ \case
    Bind TyVoid name -> throwError $ IllegalBinding name Void kind currentFunc

    Bind ty     name -> do
      vars <- gets vars
      when (M.member (name, kind) vars)
        $ throwError (IllegalBinding name Duplicate kind currentFunc)
      modify $ \env -> env { vars = M.insert (name, kind) ty vars }
      pure $ Bind ty name

checkFields :: Struct -> Semant Struct
checkFields s@(Struct name fields) = do
  fields' <- foldM
    (\acc field@(Bind t name) -> case t of
      TyVoid -> throwError $ IllegalBinding name Void StructField (S s)
      _      -> if M.member name acc
        then throwError (IllegalBinding name Duplicate StructField (S s))
        else pure $ M.insert name field acc
    )
    M.empty
    fields
  pure $ Struct name (M.elems fields') -- this doesn't preserve ordering


builtIns :: Funcs
builtIns = M.fromList $ map
  toFunc
  [ ("printf"  , [Pointer TyChar], TyVoid)
  , ("printbig", [TyInt]         , TyVoid)
  , ("malloc"  , [TyInt]         , Pointer TyVoid)
  , ("free"    , [Pointer TyVoid], TyVoid)
  ]
 where
  toFunc (name, tys, retty) =
    (name, Function retty name (map (flip Bind "x") tys) [] [])

checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  Literal  i -> pure (TyInt, SLiteral i)
  Fliteral f -> pure (TyFloat, SFliteral f)
  BoolLit  b -> pure (TyBool, SBoolLit b)
  CharLit  c -> pure (TyChar, SCharLit c)
  StrLit   s -> pure (Pointer TyChar, SStrLit s)
  Sizeof   t -> pure (TyInt, SSizeof t)
  Null       -> pure (Pointer TyVoid, SNull)
  Noexpr     -> pure (TyVoid, SNoexpr)

  Id s       -> do
    vars <- gets vars
    let foundVars = map (\kind -> M.lookup (s, kind) vars) [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol s Var expr
      Just ty -> pure (ty, LVal $ SId s)

  Binop op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs

    let assertSym  = unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)
        checkArith = do
          unless (isNumeric t1)
            $ throwError (TypeError [TyInt, TyFloat] t1 (Expr expr))
          pure (t1, SBinop op lhs' rhs')

        checkBool = do
          unless (t1 == TyBool) (throwError $ TypeError [TyBool] t1 (Expr expr))
          pure (t1, SBinop op lhs' rhs')
    case op of
      Add ->
        let sexpr = SBinop Add lhs' rhs'
        in
          case (t1, t2) of
            (Pointer t, TyInt    ) -> pure (Pointer t, sexpr)
            (TyInt    , Pointer t) -> pure (Pointer t, sexpr)
            (TyInt    , TyInt    ) -> pure (TyInt, sexpr)
            (TyFloat  , TyFloat  ) -> pure (TyFloat, sexpr)
            _ ->
              throwError $ TypeError [Pointer TyVoid, TyInt, TyFloat] t1 (Expr expr)
      Sub ->
        let sexpr = SBinop Sub lhs' rhs'
        in
          case (t1, t2) of
            (Pointer t, TyInt     ) -> pure (Pointer t, sexpr)
            (TyInt    , Pointer t ) -> pure (Pointer t, sexpr)
            (Pointer t, Pointer t') -> if t == t'
              then pure (TyInt, sexpr)
              else throwError $ TypeError [Pointer t'] (Pointer t) (Expr expr)
            (TyInt  , TyInt  ) -> pure (TyInt, sexpr)
            (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
            _                  -> throwError
              $ TypeError [Pointer TyVoid, TyInt, TyFloat] t1 (Expr expr)

      Mult   -> assertSym >> checkArith
      Div    -> assertSym >> checkArith
      BitAnd -> assertSym >> checkArith
      BitOr  -> assertSym >> checkArith
      And    -> assertSym >> checkBool
      Or     -> assertSym >> checkBool
      -- Power operator no longer exists in Sast
      Power  -> do
        assertSym
        unless (t1 == TyFloat) (throwError $ TypeError [TyFloat] t1 (Expr expr))
        pure (TyFloat, SCall "llvm.pow" [lhs', rhs'])

      relational -> case (snd lhs', snd rhs') of
        (SNull, _    ) -> checkExpr (Binop relational (Cast t1 lhs) rhs)
        (_    , SNull) -> checkExpr (Binop relational lhs (Cast t1 rhs))
        _              -> do
          assertSym
          unless (isNumeric t1)
            $ throwError (TypeError [TyInt, TyFloat] t1 (Expr expr))
          pure (TyBool, SBinop op lhs' rhs')

  Unop op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> do
        unless (isNumeric ty)
          $ throwError (TypeError [TyInt, TyFloat] ty (Expr expr))
        pure (ty, SUnop Neg e')
      Not -> do
        unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty (Expr expr)
        pure (ty, SUnop Not e')
  Addr e -> do
    (t, e') <- checkExpr e
    case e' of
      LVal l -> pure (Pointer t, SAddr l)
      _      -> throwError (AddressError e)

  Deref e -> do
    (ty, e') <- checkExpr e
    case ty of
      Pointer t -> pure (t, LVal $ SDeref (ty, e'))
      _         -> throwError
        $ TypeError [Pointer TyVoid, Pointer TyInt, Pointer TyFloat] ty (Expr expr)

  Call "printf" es -> do
    es' <- mapM checkExpr es
    let (formatStr, _) = head es'
    unless (formatStr == Pointer TyChar)
      $ throwError (TypeError [Pointer TyChar] formatStr (Expr expr))
    pure (TyVoid, SCall "printf" es')

  Call s es -> do
    funcs <- gets funcs
    case M.lookup s funcs of
      Nothing -> throwError $ UndefinedSymbol s Func expr
      Just f  -> do
        es' <- mapM checkExpr es
        -- Check that the correct number of arguments was provided
        let nFormals = length (formals f)
            nActuals = length es
        unless (nFormals == nActuals) $ throwError (ArgError nFormals nActuals expr)
        -- Check that types of arguments match
        forM_ (zip (map fst es') (map bindType (formals f)))
          $ \(callSite, defSite) ->
              unless (callSite == defSite) $ throwError $ TypeError
                { expected = [defSite]
                , got      = callSite
                , errorLoc = Expr expr
                }
        pure (typ f, SCall s es')

  Cast t e -> do
    e'@(t', _) <- checkExpr e
    case (t, t') of
      (Pointer _, Pointer _) -> pure (t, SCast t e')
      (Pointer _, TyInt    ) -> pure (t, SCast t e')
      _                      -> throwError $ CastError t t' (Expr expr)

  Access e field -> do
    fieldName <- case field of
      Id f -> pure f
      _    -> throwError (AccessError field e)

    (t, e') <- checkExpr e
    l       <- case e' of
      LVal l' -> pure l'
      _       -> throwError (AccessError e field)
    (Struct _ fields) <- case t of
      TyStruct name' -> do
        ss <- gets structs
        case find (\(Struct n _) -> n == name') ss of
          Nothing -> throwError (TypeError [TyStruct "a_struct"] t (Expr expr))
          Just s  -> pure s
      _ -> throwError (TypeError [TyStruct "a_struct"] t (Expr expr))

    f <- case findIndex (\(Bind _ f) -> f == fieldName) fields of
      Nothing -> throwError (AccessError e field)
      Just i  -> pure i

    pure (bindType (fields !! f), LVal $ SAccess l f)

  Assign lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    let assertSym = unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)

    lval <- case snd lhs' of
      LVal e -> pure e
      _      -> throwError $ AssignmentError lhs rhs
    case snd rhs' of
      SNull -> checkExpr (Assign lhs (Cast t1 rhs))
      _     -> (t2, SAssign lval rhs') <$ assertSym
 where
  isNumeric = \case
    TyInt     -> True
    TyFloat   -> True
    TyChar    -> True
    Pointer _ -> True
    _         -> False


checkStatement :: Statement -> Semant SStatement
checkStatement stmt = case stmt of
  Expr e           -> SExpr <$> checkExpr e

  If pred cons alt -> do
    pred'@(ty, _) <- checkExpr pred
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    SIf pred' <$> checkStatement cons <*> checkStatement alt

  For init cond inc action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    init'   <- checkExpr init
    inc'    <- checkExpr inc
    action' <- checkStatement action
    pure $ SFor init' cond' inc' action'

  While cond action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmt
    SWhile cond' <$> checkStatement action

  Return expr -> do
    e@(ty, _) <- checkExpr expr
    fun       <- gets thisFunc
    unless (ty == typ fun) $ throwError $ TypeError [typ fun] ty stmt
    pure $ SReturn e

  Block sl -> do
    let flattened = flatten sl
    unless (nothingFollowsRet flattened) $ throwError (DeadCode stmt)
    SBlock <$> mapM checkStatement sl
   where
    flatten []             = []
    flatten (Block s : ss) = flatten (s ++ ss)
    flatten (s       : ss) = s : flatten ss

    nothingFollowsRet []         = True
    nothingFollowsRet [Return _] = True
    nothingFollowsRet (s : ss  ) = case s of
      Return _ -> False
      _        -> nothingFollowsRet ss

checkFunction :: Function -> Semant SFunction
checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- gets funcs
  unless (M.notMember (name func) funcs) $ throwError $ Redeclaration (name func)
  -- add this func to symbol table
  modify $ \env -> env { funcs = M.insert (name func) func funcs, thisFunc = func }

  (formals', locals', body') <- locally $ liftM3
    (,,)
    (checkBinds Formal (formals func))
    (checkBinds Local (locals func))
    (checkStatement (Block $ body func))

  case body' of
    SBlock body'' -> do
      unless (typ func == TyVoid || validate (genCFG body''))
        $ throwError (TypeError [typ func] TyVoid (Block $ body func))

      pure $ SFunction { styp     = typ func
                       , sname    = name func
                       , sformals = formals'
                       , slocals  = locals'
                       , sbody    = SBlock body''
                       }
    _ -> error "Internal error - block didn't become a block?"

checkProgram :: Program -> Either SemantError SProgram
checkProgram (Program structs binds funcs) = evalState
  (runExceptT (checkProgram' (structs, binds, funcs)))
  baseEnv
 where
  baseEnv =
    Env { structs = [], vars = M.empty, funcs = builtIns, thisFunc = garbageFunc }

  garbageFunc =
    Function { typ = TyVoid, name = "", formals = [], locals = [], body = [] }

  checkProgram' (structs, binds, funcs) = do
    structs' <- mapM checkFields structs
    modify $ \e -> e { structs = structs' }
    globals <- checkBinds Global binds
    funcs'  <- mapM checkFunction funcs
    case find (\f -> sname f == "main") funcs' of
      Nothing -> throwError NoMain
      Just _  -> pure (structs', globals, funcs')
