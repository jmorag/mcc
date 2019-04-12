{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Microc.Utils
import           Microc.Sast
import           Microc.Ast                     ( Type(..)
                                                , Op(..)
                                                , Uop(..)
                                                , Bind(..)
                                                )

import           Data.String.Conversions
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

-- When using the IRBuilder, both functions and variables have the type Operand
type Env = M.Map Text AST.Operand

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack

ltypeOfTyp :: Type -> AST.Type
ltypeOfTyp TyVoid  = AST.void
ltypeOfTyp TyInt   = AST.i32
ltypeOfTyp TyFloat = AST.double
ltypeOfTyp TyBool  = AST.IntegerType 1

charStar :: AST.Type
charStar = AST.ptr $ AST.IntegerType 8

codegenSexpr :: SExpr -> Codegen AST.Operand
codegenSexpr (TyInt  , SLiteral i ) = L.int32 (fromIntegral i)
codegenSexpr (TyFloat, SFliteral f) = L.double f
codegenSexpr (TyBool , SBoolLit b ) = L.bit (if b then 1 else 0)
codegenSexpr (_      , SId name   ) = do
  vars <- get
  case M.lookup name vars of
    Just addr -> L.load addr 0
    Nothing -> error . cs $ "Internal error - undefined variable name " <> name

codegenSexpr (TyInt, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of
      Add    -> L.add
      Sub    -> L.sub
      Mult   -> L.mul
      Div    -> L.sdiv
      _      -> error "Internal error - semant failed"
    )
    lhs'
    rhs'
codegenSexpr (TyFloat, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of
      Add    -> L.fadd
      Sub    -> L.fsub
      Mult   -> L.fmul
      Div    -> L.fdiv
      _      -> error "Internal error - semant failed"
    )
    lhs'
    rhs'
codegenSexpr (TyBool, SBinop op lhs@(TyInt, _) rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of
      Equal   -> L.icmp IP.EQ
      Neq     -> L.icmp IP.NE
      Less    -> L.icmp IP.SLT
      Leq     -> L.icmp IP.SLE
      Greater -> L.icmp IP.SGT
      Geq     -> L.icmp IP.SGE
      _       -> error "Internal error - semant failed"
    )
    lhs'
    rhs'
codegenSexpr (TyBool, SBinop op lhs@(TyFloat, _) rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of
      Equal   -> L.fcmp FP.OEQ
      Neq     -> L.fcmp FP.ONE
      Less    -> L.fcmp FP.OLT
      Leq     -> L.fcmp FP.OLE
      Greater -> L.fcmp FP.OGT
      Geq     -> L.fcmp FP.OGE
      _       -> error "Internal error - semant failed"
    )
    lhs'
    rhs'
codegenSexpr (TyBool, SBinop op lhs@(TyBool, _) rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of
      And   -> L.and
      Or    -> L.or
      Equal -> L.icmp IP.EQ
      Neq   -> L.icmp IP.NE
      _     -> error "Internal error - semant failed"
    )
    lhs'
    rhs'

-- The Haskell LLVM bindings don't provide numerical or boolean negation
-- primitives, but they're easy enough to emit ourselves
codegenSexpr (TyInt, SUnop Neg e) = do
  zero <- L.int32 0
  e'   <- codegenSexpr e
  L.sub zero e'
codegenSexpr (TyFloat, SUnop Neg e) = do
  zero <- L.double 0
  e'   <- codegenSexpr e
  L.fsub zero e'
codegenSexpr (TyBool, SUnop Not e) = do
  true <- L.bit 1
  e'   <- codegenSexpr e
  L.xor true e'

codegenSexpr (_, SAssign name e) = do
  addr <- gets (M.! name)
  e'   <- codegenSexpr e
  L.store addr 0 e'
  return e'

codegenSexpr (_, SCall fun es) = do
  intFormatStr   <- gets (M.! "_intFmt")
  floatFormatStr <- gets (M.! "_floatFmt")
  printf         <- gets (M.! "printf")
  case fun of
    "print" -> do
      e' <- codegenSexpr (head es)
      L.call printf [(intFormatStr, []), (e', [])]
    "printf" -> do
      e' <- codegenSexpr (head es)
      L.call printf [(floatFormatStr, []), (e', [])]
    "printb" -> do
      e' <- codegenSexpr (head es)
      L.call printf [(intFormatStr, []), (e', [])]
    _ -> do
      es' <- forM es $ \e -> do
        e' <- codegenSexpr e
        return (e', [])
      f <- gets (M.! fun)
      L.call f es'

codegenSexpr (_, SNoexpr) = L.int32 0

-- Final catchall
codegenSexpr sx =
  error $ "Internal error - semant failed. Invalid sexpr " <> show sx


codegenStatement :: SStatement -> Codegen ()
codegenStatement (SExpr   e) = void $ codegenSexpr e

codegenStatement (SReturn e) = case e of
  (TyVoid, SNoexpr) -> L.retVoid
  _                 -> codegenSexpr e >>= L.ret

codegenStatement (SBlock ss        ) = mapM_ codegenStatement ss

codegenStatement (SIf pred cons alt) = mdo
  bool <- codegenSexpr pred
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
-- implement while as `if pred //enter do while// else leave`
codegenStatement (SWhile pred body) = mdo
  -- check the condition the first time
  bool <- codegenSexpr pred
  L.condBr bool whileBlock mergeBlock
  whileBlock <- L.block `L.named` "while_body"
  do
    codegenStatement body
    -- Make sure that there was no return inside of the block and then generate
    -- the check on the condition and go back to the beginning
    check <- L.hasTerminator
    unless check $ do
      continue <- codegenSexpr pred
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
  modify $ M.insert (sname f) fun
  -- We wrap generating the function inside of the `locally` combinator in
  -- order to prevent local variables from escaping the scope of the function
  fun <-
    locally
      $ let name = mkName (cs $ sname f)
            mkParam (Bind t n) = (ltypeOfTyp t, L.ParameterName (cs n))
            args  = map mkParam (sformals f)
            retty = ltypeOfTyp (styp f)

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
                modify $ M.insert n addr
              -- Same for the locals, except we do not emit the store instruction for
              -- them
              forM_ (slocals f) $ \(Bind t n) -> do
                let ltype = ltypeOfTyp t
                addr <- L.alloca ltype Nothing 0
                modify $ M.insert n addr
              -- Evaluate the actual body of the function after making the necessary
              -- allocations
              codegenStatement (sbody f)

        in  L.function name args retty body
  return ()

emitBuiltIns :: LLVM ()
emitBuiltIns = do
  printbig <- L.extern (mkName "printbig") [AST.i32] AST.void
  printf   <- L.externVarArgs (mkName "printf") [charStar] AST.i32
  modify $ M.insert "printf" printf
  modify $ M.insert "printbig" printbig
  intFmt   <- L.globalStringPtr "%d\n" $ mkName "_intFmt"
  floatFmt <- L.globalStringPtr "%g\n" $ mkName "_floatFmt"
  modify $ M.insert "_intFmt" intFmt
  modify $ M.insert "_floatFmt" floatFmt

codegenGlobal :: Bind -> LLVM ()
codegenGlobal (Bind t n) = do
  let name    = mkName $ cs n
      typ     = ltypeOfTyp t
      initVal = C.Int 0 0
  var <- L.global name typ initVal
  modify $ M.insert n var

codegenProgram :: SProgram -> AST.Module
codegenProgram (globals, funcs) =
  flip evalState M.empty $ L.buildModuleT "microc" $ do
    emitBuiltIns
    mapM_ codegenGlobal globals
    mapM_ codegenFunc   funcs
