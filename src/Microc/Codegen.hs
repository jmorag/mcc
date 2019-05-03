{-# LANGUAGE NamedFieldPuns #-}
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
import qualified LLVM.AST                      as AST
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
data Env = Env { operands :: M.Map Text AST.Operand
               , structs :: [ Struct ]
               , strings :: (M.Map Text AST.Operand, Int)
               }
  deriving (Eq, Show)

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

registerOperand :: MonadState Env m => Text -> AST.Operand -> m ()
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
  Pointer TyVoid -> pure $ AST.ptr AST.i8
  -- special case to handle recursively defined structures
  Pointer (TyStruct n) ->
    pure $ AST.ptr (AST.NamedTypeReference (mkName $ cs ("struct." <> n)))
  Pointer  t -> fmap AST.ptr (ltypeOfTyp t)
  TyStruct n -> do
    fields <- getFields n
    typs   <- mapM (ltypeOfTyp . bindType) fields
    -- Packed structs aren't great for performance but very easy to code for now
    pure $ AST.StructureType {AST.isPacked = True, AST.elementTypes = typs}

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
  TyStruct n -> fmap sum $ mapM (sizeof . bindType) =<< getFields n

codegenLVal :: LValue -> Codegen AST.Operand
codegenLVal (SId    name) = gets ((M.! name) . operands)
codegenLVal (SDeref e   ) = codegenSexpr e

codegenLVal (SAccess e i) = do
  e'     <- codegenLVal e
  zero   <- L.int32 0
  offset <- L.int32 (fromIntegral i)
  L.gep e' [zero, offset]

codegenSexpr :: SExpr -> Codegen AST.Operand
codegenSexpr (TyInt  , SLiteral i ) = L.int32 (fromIntegral i)
codegenSexpr (TyFloat, SFliteral f) = L.double f
codegenSexpr (TyBool , SBoolLit b ) = L.bit (if b then 1 else 0)
codegenSexpr (TyChar, SCharLit c) =
  pure $ AST.ConstantOperand (C.Int 8 (fromIntegral c))
codegenSexpr (Pointer TyChar, SStrLit s) = do
  -- Generate a new unique global variable for every string literal we see
  (strs, count) <- gets strings
  case M.lookup s strs of
    Nothing -> do
      let nm = mkName (cs (".str" ++ show count))
      op <- L.globalStringPtr (cs s) nm
      modify $ \env -> env { strings = (M.insert s op strs, count + 1) }
      pure op
    Just op -> pure op

codegenSexpr (t, SNull) =
  L.inttoptr (AST.ConstantOperand $ C.Int 64 0) =<< ltypeOfTyp t
codegenSexpr (TyInt, SSizeof t      ) = L.int32 =<< fromIntegral <$> sizeof t

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
    Add -> case t of
      TyInt     -> L.add lhs' rhs'
      TyFloat   -> L.fadd lhs' rhs'
      Pointer _ -> case (fst lhs, fst rhs) of
        (Pointer _, TyInt    ) -> L.gep lhs' [rhs']
        (TyInt    , Pointer _) -> L.gep rhs' [lhs']
        _                      -> error "Internal error - semant failed"
      _ -> error "Internal error - semant failed"
    Sub -> case t of
      TyInt -> case (fst lhs, fst rhs) of
        (TyInt      , TyInt    ) -> L.sub lhs' rhs'
        (Pointer typ, Pointer _) -> do
          lhs'' <- L.ptrtoint lhs' AST.i64
          rhs'' <- L.ptrtoint rhs' AST.i64
          diff  <- L.sub lhs'' rhs''
          width <- L.int64 . fromIntegral =<< sizeof typ
          L.sdiv diff width
        _ -> error "Internal error - semant failed"
      TyFloat   -> L.fsub lhs' rhs'
      Pointer _ -> do
        zero <- L.int64 0
        case (fst lhs, fst rhs) of
          (Pointer _, TyInt) ->
            L.zext rhs' AST.i64 >>= L.sub zero >>= L.gep lhs' . (: [])
          (TyInt, Pointer _) ->
            L.zext lhs' AST.i64 >>= L.sub zero >>= L.gep rhs' . (: [])
          _ -> error "Internal error - semant failed"
      _ -> error "Internal error - semant failed"
    Mult -> case t of
      TyInt   -> L.mul lhs' rhs'
      TyFloat -> L.fmul lhs' rhs'
      _       -> error "Internal error - semant failed"
    Div -> case t of
      TyInt   -> L.sdiv lhs' rhs'
      TyFloat -> L.fdiv lhs' rhs'
      _       -> error "Internal error - semant failed"
    Power ->
      error "Internal error - Power should have been eliminated in semant"
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
      TyInt   -> L.int32 0 >>= flip L.sub e'
      TyFloat -> L.double 0 >>= flip L.fsub e'
      _       -> error "Internal error - semant failed"
    Not -> case t of
      TyBool -> L.bit 1 >>= L.xor e'
      _      -> error "Internal error - semant failed"

codegenSexpr (_, SCall fun es) = do
  es' <- forM es $ \e -> do
    e' <- codegenSexpr e
    return (e', [])
  f <- gets ((M.! fun) . operands)
  L.call f es'

codegenSexpr (_, SCast t e) = do
  e' <- codegenSexpr e
  L.bitcast e' =<< ltypeOfTyp t

codegenSexpr (_, SNoexpr) = L.int32 0

-- Final catchall
codegenSexpr sx =
  error $ "Internal error - semant failed. Invalid sexpr " <> show sx


codegenStatement :: SStatement -> Codegen ()
codegenStatement (SExpr   e) = void $ codegenSexpr e

codegenStatement (SReturn e) = case e of
  (TyVoid, SNoexpr) -> L.retVoid
  _                 -> codegenSexpr e >>= L.ret

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

-- Implementing a do-while construct is actually easier than while, so we
-- implement while as `if p //enter do while// else leave`
codegenStatement (SWhile p body) = mdo
  -- check the condition the first time
  bool <- codegenSexpr p
  L.condBr bool whileBlock mergeBlock
  whileBlock <- L.block `L.named` "while_body"
  do
    codegenStatement body
    -- Make sure that there was no return inside of the block and then generate
    -- the check on the condition and go back to the beginning
    check <- L.hasTerminator
    unless check $ do
      continue <- codegenSexpr p
      L.condBr continue whileBlock mergeBlock
  mergeBlock <- L.block `L.named` "merge"
  return ()

-- Turn for loops into equivalent while loops
codegenStatement (SFor e1 e2 e3 body) = codegenStatement newStatement
 where
  newStatement = SBlock [SExpr e1, SWhile e2 body']
  body'        = SBlock [body, SExpr e3]


mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr

-- | Generate a function and add both the function name and variable names to
-- the map. TODO document wtf is going on in here
codegenFunc :: SFunction -> LLVM ()
codegenFunc f = mdo
  -- We need to forward reference the generated function and insert it into the
  -- environment _before_ generating its body in order to handle the
  -- possibility of the function calling itself recursively
  registerOperand (sname f) fun
  -- We wrap generating the function inside of the `locally` combinator in
  -- order to prevent local variables from escaping the scope of the function
  fun <- locally' $ do
    retty <- ltypeOfTyp (styp f)
    let name = mkName (cs $ sname f)
        mkParam (Bind t n) =
          ltypeOfTyp t >>= \t' -> return (t', L.ParameterName (cs n))

        -- Generate the body of the function:
        body ops = do
          let pairs = zip ops (sformals f)
          _entry <- L.block `L.named` "entry"
          -- Add the formal parameters to the map, allocate them on the stack,
          -- and then emit the necessary store instructions
          forM_ pairs $ \(op, Bind _ n) -> do
            let ltype = typeOf op
            addr <- L.alloca ltype Nothing 0
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
    args <- mapM mkParam (sformals f)
    L.function name args retty body
  return ()
 where
    -- Slightly different version of the locally combinator
    -- In this case we actually want to persist global string variables across
    -- different functions.
  locally' codegen = do
    ops    <- gets operands
    result <- codegen
    modify $ \e -> e { operands = ops }
    return result

emitBuiltIns :: LLVM ()
emitBuiltIns = do
  printbig <- L.extern (mkName "printbig") [AST.i32] AST.void
  printf   <- L.externVarArgs (mkName "printf") [charStar] AST.i32
  registerOperand "printf"   printf
  registerOperand "printbig" printbig
  llvmPow <- L.extern (mkName "llvm.pow.f64")
                      [AST.double, AST.double]
                      AST.double
  registerOperand "llvm.pow" llvmPow

  malloc <- L.extern (mkName "malloc") [AST.i32] (AST.ptr AST.i8)
  registerOperand "malloc" malloc
  free <- L.extern (mkName "free") [AST.ptr AST.i8] AST.void
  registerOperand "free" free

codegenGlobal :: Bind -> LLVM ()
codegenGlobal (Bind t n) = do
  let name    = mkName $ cs n
      initVal = C.Int 0 0
  typ <- ltypeOfTyp t
  var <- L.global name typ initVal
  registerOperand n var

emitTypeDef
  :: (MonadState Env m, L.MonadModuleBuilder m) => Struct -> m AST.Type
emitTypeDef (Struct name _) = do
  typ <- ltypeOfTyp (TyStruct name)
  L.typedef (mkName (cs ("struct." <> name))) (Just typ)


codegenProgram :: SProgram -> AST.Module
codegenProgram (structs, globals, funcs) =
  flip evalState
       (Env {operands = M.empty, structs = structs, strings = (M.empty, 0)})
    $ L.buildModuleT "microc"
    $ do
        emitBuiltIns
        mapM_ emitTypeDef   structs
        mapM_ codegenGlobal globals
        mapM_ codegenFunc   funcs
