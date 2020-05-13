{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Microc.Codegen
  ( codegenProgram
  )
where

import qualified LLVM.AST.IntegerPredicate     as IP
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import           LLVM.AST                       ( Operand )
import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Float                as AST
import qualified LLVM.AST.Type                 as AST
import qualified LLVM.AST.Constant             as C
import           LLVM.AST.Name
import           LLVM.AST.Typed                 ( typeOf )

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L
import           LLVM.Prelude                   ( ShortByteString )

import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.String                    ( fromString )

import           Microc.Utils
import           Microc.Sast
import           Microc.Ast                     ( Type(..)
                                                , Op(..)
                                                , Uop(..)
                                                , Bind(..)
                                                , Struct(..)
                                                )

import           Data.String.Conversions
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Word                      ( Word32 )
import           Data.List                      ( find )
import           Debug.Trace

-- When using the IRBuilder, both functions and variables have the type Operand
data Env = Env { operands :: M.Map Text Operand
               , structs :: [ Struct ]
               , strings :: M.Map Text Operand
               }
  deriving (Eq, Show)

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

getFields :: MonadState Env m => Text -> m [Bind]
getFields name = do
  ss <- gets structs
  case find (\s -> structName s == name) ss of
    Nothing               -> error "Internal error - struct not found"
    Just (Struct _ binds) -> pure binds

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack

ltypeOfTyp :: MonadState Env m => Type -> m AST.Type
ltypeOfTyp = \case
  TyVoid         -> pure AST.void
  TyInt          -> pure AST.i32
  TyChar         -> pure AST.i8
  TyFloat        -> pure AST.double
  TyBool         -> pure AST.i1
  -- (void *) is invalid LLVM
  Pointer TyVoid -> pure $ charStar
  -- special case to handle recursively defined structures
  -- TODO: add real cycle checking so that improperly defined
  -- recursive types case the compiler to hang forever
  Pointer (TyStruct n) ->
    pure $ AST.ptr (AST.NamedTypeReference (mkName $ cs ("struct." <> n)))
  Pointer  t -> fmap AST.ptr (ltypeOfTyp t)
  TyStruct n -> do
    fields <- getFields n
    typs   <- mapM (ltypeOfTyp . bindType) fields
    -- Packed structs aren't great for performance but very easy to code for now
    pure $ AST.StructureType { AST.isPacked = True, AST.elementTypes = typs }

charStar :: AST.Type
charStar = AST.ptr AST.i8

sizeof :: MonadState Env m => Type -> m Word32
sizeof = \case
  TyBool     -> pure 1
  TyChar     -> pure 1
  TyInt      -> pure 4
  TyFloat    -> pure 8
  TyVoid     -> pure 0
  Pointer  _ -> pure 8
  TyStruct n -> do
    fields <- getFields n
    sizes  <- mapM (sizeof . bindType) fields
    pure (sum sizes)

codegenLVal :: LValue -> Codegen Operand
codegenLVal (SId    name) = gets ((M.! name) . operands)
codegenLVal (SDeref e   ) = codegenSexpr e

codegenLVal (SAccess e i) = do
  e' <- codegenLVal e
  let zero   = L.int32 0
      offset = L.int32 (fromIntegral i)
  L.gep e' [zero, offset]

codegenSexpr :: SExpr -> Codegen Operand
codegenSexpr (TyInt         , SLiteral i ) = pure $ L.int32 (fromIntegral i)
codegenSexpr (TyFloat       , SFliteral f) = pure $ L.double f
codegenSexpr (TyBool        , SBoolLit b ) = pure $ L.bit (if b then 1 else 0)
codegenSexpr (TyChar        , SCharLit c ) = pure $ L.int8 (fromIntegral c)
codegenSexpr (Pointer TyChar, SStrLit s  ) = do
  -- Generate a new unique global variable for every string literal we see
  strs <- gets strings
  case M.lookup s strs of
    Nothing -> do
      let nm = mkName (show (M.size strs) <> ".str")
      op <- L.globalStringPtr (cs s) nm
      modify $ \env -> env { strings = M.insert s (AST.ConstantOperand op) strs }
      pure (AST.ConstantOperand op)
    Just op -> pure op

codegenSexpr (t    , SNull          ) = L.inttoptr (L.int64 0) =<< ltypeOfTyp t
codegenSexpr (TyInt, SSizeof t      ) = L.int32 . fromIntegral <$> sizeof t

-- All LVals are already memory addresses.
codegenSexpr (_    , SAddr e        ) = codegenLVal e
codegenSexpr (_    , LVal e         ) = flip L.load 0 =<< codegenLVal e
codegenSexpr (_    , SAssign lhs rhs) = do
  rhs' <- codegenSexpr rhs
  lhs' <- codegenLVal lhs
  L.store lhs' 0 rhs'
  return rhs'
codegenSexpr (t, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  case op of
    Add -> case (fst lhs, fst rhs) of
      (Pointer _, TyInt    ) -> L.gep lhs' [rhs']
      (TyInt    , Pointer _) -> L.gep rhs' [lhs']
      (TyInt    , TyInt    ) -> L.add lhs' rhs'
      (TyFloat  , TyFloat  ) -> L.fadd lhs' rhs'
      _                      -> error "Internal error - semant failed"
    Sub ->
      let zero = L.int32 0
      in  case (fst lhs, fst rhs) of
            (Pointer typ, Pointer typ') -> if typ' /= typ
              then error "Internal error - semant failed"
              else do
                lhs'' <- L.ptrtoint lhs' AST.i64
                rhs'' <- L.ptrtoint rhs' AST.i64
                diff  <- L.sub lhs'' rhs''
                width <- L.int64 . fromIntegral <$> sizeof typ
                result <- L.sdiv diff width
                L.trunc result AST.i32
            (Pointer _, TyInt) -> do
              rhs'' <- L.sub zero rhs'
              L.gep lhs' [rhs'']
            (TyInt  , TyInt  ) -> L.sub lhs' rhs'
            (TyFloat, TyFloat) -> L.fsub lhs' rhs'
            _                  -> error "Internal error - semant failed"
    Mult -> case t of
      TyInt   -> L.mul lhs' rhs'
      TyFloat -> L.fmul lhs' rhs'
      _       -> error "Internal error - semant failed"
    Div -> case t of
      TyInt   -> L.sdiv lhs' rhs'
      TyFloat -> L.fdiv lhs' rhs'
      _       -> error "Internal error - semant failed"
    -- We implement int ** int directly in llvm
    Power -> mdo
      enclosing <- L.currentBlock
      L.br loop
      loop <- L.block `L.named` "loop_pow"
      acc <- L.phi [(L.int32 1, enclosing), (nextAcc, continueBlock)] `L.named` "acc"
      expt <- L.phi [(rhs', enclosing), (nextExpt, continueBlock)] `L.named` "expt"
      done <- L.icmp IP.EQ expt (L.int32 0)
      L.condBr done doneBlock continueBlock
      continueBlock <- L.block `L.named` "continue"
      nextAcc       <- L.mul acc lhs' `L.named` "next_acc"
      nextExpt      <- L.sub expt (L.int32 1) `L.named` "next_expt"
      L.br loop
      doneBlock <- L.block `L.named` "done"
      pure acc

    -- Only relational operators defined on chars, not arithmetic
    Equal -> case fst lhs of
      TyInt     -> L.icmp IP.EQ lhs' rhs'
      TyBool    -> L.icmp IP.EQ lhs' rhs'
      TyChar    -> L.icmp IP.EQ lhs' rhs'
      Pointer _ -> L.icmp IP.EQ lhs' rhs'
      TyFloat   -> L.fcmp FP.OEQ lhs' rhs'
      _         -> error "Internal error - semant failed"
    Neq -> case fst lhs of
      TyInt     -> L.icmp IP.NE lhs' rhs'
      TyBool    -> L.icmp IP.NE lhs' rhs'
      TyChar    -> L.icmp IP.NE lhs' rhs'
      Pointer _ -> L.icmp IP.NE lhs' rhs'
      TyFloat   -> L.fcmp FP.ONE lhs' rhs'
      _         -> error "Internal error - semant failed"
    Less -> case fst lhs of
      TyInt   -> L.icmp IP.SLT lhs' rhs'
      TyBool  -> L.icmp IP.SLT lhs' rhs'
      TyChar  -> L.icmp IP.ULT lhs' rhs'
      TyFloat -> L.fcmp FP.OLT lhs' rhs'
      _       -> error "Internal error - semant failed"
    Leq -> case fst lhs of
      TyInt   -> L.icmp IP.SLE lhs' rhs'
      TyBool  -> L.icmp IP.SLE lhs' rhs'
      TyChar  -> L.icmp IP.ULE lhs' rhs'
      TyFloat -> L.fcmp FP.OLE lhs' rhs'
      _       -> error "Internal error - semant failed"
    Greater -> case fst lhs of
      TyInt   -> L.icmp IP.SGT lhs' rhs'
      TyBool  -> L.icmp IP.SGT lhs' rhs'
      TyChar  -> L.icmp IP.UGT lhs' rhs'
      TyFloat -> L.fcmp FP.OGT lhs' rhs'
      _       -> error "Internal error - semant failed"
    Geq -> case fst lhs of
      TyInt   -> L.icmp IP.SGE lhs' rhs'
      TyBool  -> L.icmp IP.SGE lhs' rhs'
      TyChar  -> L.icmp IP.UGE lhs' rhs'
      TyFloat -> L.fcmp FP.OGE lhs' rhs'
      _       -> error "Internal error - semant failed"
    -- Relational operators all emit the same instructions
    -- Calling And between floats or pointers doesn't make sense,
    -- but semant catches that. Calling BitAnd between them IS allowed,
    -- and makes even less sense, but this is C, and we "trust" programmers
    -- to know why they want to do such things.
    And    -> L.and lhs' rhs'
    Or     -> L.or lhs' rhs'
    BitAnd -> L.and lhs' rhs'
    BitOr  -> L.or lhs' rhs'

codegenSexpr (t, SUnop op e) = do
  e' <- codegenSexpr e
  case op of
    Neg -> case t of
      TyInt   -> L.sub (L.int32 0) e'
      TyFloat -> L.fsub (L.double 0) e'
      _       -> error "Internal error - semant failed"
    Not -> case t of
      TyBool -> L.xor e' (L.bit 1)
      _      -> error "Internal error - semant failed"

codegenSexpr (_, SCall fun es) = do
  es' <- mapM (fmap (, []) . codegenSexpr) es
  f   <- gets ((M.! fun) . operands)
  L.call f es'

codegenSexpr (_, SCast t' e@(t, _)) = do
  e'       <- codegenSexpr e
  llvmType <- ltypeOfTyp t'
  case (t', t) of
    (Pointer _, Pointer _) -> L.bitcast e' llvmType
    (Pointer _, TyInt    ) -> L.inttoptr e' llvmType
    (TyInt    , Pointer _) -> L.ptrtoint e' llvmType
    (TyFloat  , TyInt    ) -> L.sitofp e' llvmType
    _ -> error $ "Internal error - semant failed. Invalid sexpr " <> show
      (t', SCast t e)

codegenSexpr (_, SNoexpr) = pure $ L.int64 0

-- Final catchall
codegenSexpr sx =
  error $ "Internal error - semant failed. Invalid sexpr " <> show sx


codegenStatement :: SStatement -> Codegen ()
codegenStatement (SExpr   e) = void $ codegenSexpr e

codegenStatement (SReturn e) = case e of
  (TyVoid, SNoexpr) -> L.retVoid
  _                 -> L.ret =<< codegenSexpr e

codegenStatement (SBlock ss     ) = mapM_ codegenStatement ss

codegenStatement (SIf p cons alt) = mdo
  bool <- codegenSexpr p
  L.condBr bool thenBlock elseBlock
  thenBlock <- L.block `L.named` "then"
  do
    codegenStatement cons
    mkTerminator $ L.br mergeBlock
  elseBlock <- L.block `L.named` "else"
  do
    codegenStatement alt
    mkTerminator $ L.br mergeBlock
  mergeBlock <- L.block `L.named` "merge"
  return ()

codegenStatement (SDoWhile p body) = mdo
  L.br whileBlock
  whileBlock <- L.block `L.named` "while_body"
  do
    codegenStatement body
    continue <- codegenSexpr p
    mkTerminator $ L.condBr continue whileBlock mergeBlock
  mergeBlock <- L.block `L.named` "merge"
  return ()

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr

-- | Generate a function and add both the function name and variable names to
-- the map.
codegenFunc :: SFunction -> LLVM ()
codegenFunc f = mdo
  -- We need to forward reference the generated function and insert it into the
  -- environment _before_ generating its body in order to handle the
  -- possibility of the function calling itself recursively
  registerOperand (sname f) function
  -- We wrap generating the function inside of the `locally` combinator in
  -- order to prevent local variables from escaping the scope of the function
  (function, strs) <- locally $ do
    retty <- ltypeOfTyp (styp f)
    args  <- mapM mkParam (sformals f)
    fun <- L.function name args retty genBody
    strings' <- gets strings
    pure (fun, strings')
  modify $ \e -> e { strings = strs }
 where
  name = mkName (cs $ sname f)
  mkParam (Bind t n) = (,) <$> ltypeOfTyp t <*> pure (L.ParameterName (cs n))

  -- Generate the body of the function:
  genBody :: [Operand] -> Codegen ()
  genBody ops = do
    _entry <- L.block `L.named` "entry"
    -- Add the formal parameters to the map, allocate them on the stack,
    -- and then emit the necessary store instructions
    forM_ (zip ops (sformals f)) $ \(op, Bind _ n) -> do
      addr <- L.alloca (typeOf op) Nothing 0
      L.store addr 0 op
      registerOperand n addr
    -- Same for the locals, except we do not emit the store instruction for
    -- them
    forM_ (slocals f) $ \(Bind t n) -> do
      ltype <- ltypeOfTyp t
      addr  <- L.alloca ltype Nothing 0
      registerOperand n addr
    -- Evaluate the actual body of the function after making the necessary
    -- allocations
    codegenStatement (sbody f)

emitBuiltIn :: (String, [AST.Type], AST.Type) -> LLVM ()
emitBuiltIn (name, argtys, retty) = do
  func <- L.extern (mkName name) argtys retty
  registerOperand (cs name) func

builtIns :: [(String, [AST.Type], AST.Type)]
builtIns =
  [ ("printbig"     , [AST.i32]               , AST.void)
  , ("llvm.pow.f64" , [AST.double, AST.double], AST.double)
  , ("llvm.powi.f64", [AST.double, AST.i32]   , AST.double)
  , ("malloc"       , [AST.i32]               , AST.ptr AST.i8)
  , ("free"         , [AST.ptr AST.i8]        , AST.void)
  ]

codegenGlobal :: Bind -> LLVM ()
codegenGlobal (Bind t n) = do
  typ <- ltypeOfTyp t
  let name    = mkName $ cs n
      initVal = case t of
        Pointer _ -> C.Int 64 0
        TyStruct _ -> C.AggregateZero typ
        TyInt -> C.Int 32 0
        TyBool -> C.Int 1 0
        TyFloat -> C.Float (AST.Double 0)
        TyChar -> C.Int 8 0
        TyVoid -> error "Global void variables illegal"
  var <- L.global name typ initVal
  registerOperand n var

emitTypeDef :: Struct -> LLVM AST.Type
emitTypeDef (Struct name _) = do
  typ <- ltypeOfTyp (TyStruct name)
  L.typedef (mkName (cs ("struct." <> name))) (Just typ)


codegenProgram :: SProgram -> AST.Module
codegenProgram (structs, globals, funcs) =
  flip evalState (Env { operands = M.empty, structs = structs, strings = M.empty })
    $ L.buildModuleT "microc"
    $ do
        printf <- L.externVarArgs (mkName "printf") [charStar] AST.i32
        registerOperand "printf" printf
        mapM_ emitBuiltIn builtIns
        mapM_ emitTypeDef   structs
        mapM_ codegenGlobal globals
        mapM_ codegenFunc   funcs
