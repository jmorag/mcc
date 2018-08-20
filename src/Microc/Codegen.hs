{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- These extensions necessary to convert to and from ShortByteString, which 
-- the LLVM bindings use internally for variable names
module Microc.Codegen (codegenProgram) where

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import LLVM.AST.Linkage
import LLVM.AST.Name

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import LLVM.Prelude (ShortByteString)

import qualified Data.Map as M
import Control.Monad.State
import Data.String (fromString)

import qualified Microc.Semant as Semant
import Microc.Sast
import Microc.Ast (Type(..), Op(..), Uop(..), Function(..), Bind)

import           Data.String.Conversions
import qualified Data.Text as T
import           Data.Text (Text)

import Debug.Trace

-- When using the IRBuilder, both functions and variables have the type Operand
type Env = M.Map Text AST.Operand
type LLVM = L.ModuleBuilderT (State Env)

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack

ltypeOfTyp :: Type -> AST.Type
ltypeOfTyp TyVoid = AST.void
ltypeOfTyp TyInt = AST.i32
ltypeOfTyp TyFloat = AST.double
ltypeOfTyp TyBool = AST.IntegerType 1

codegenSexpr :: (MonadState Env m, L.MonadIRBuilder m) => SExpr -> m AST.Operand
codegenSexpr (TyInt, SLiteral i) = L.int32 (fromIntegral i)
codegenSexpr (TyFloat, SFliteral f) = L.double f
codegenSexpr (TyBool, SBoolLit b) = L.bit (if b then 1 else 0)
codegenSexpr (_, SId name) = do
  vars <- get
  case M.lookup name vars of
    Just addr -> L.load addr 0
    Nothing -> error . cs $ "Internal error - undefined variable name " <> name 

codegenSexpr (TyInt, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of Add -> L.add; Sub -> L.sub; 
              Mult -> L.mul; Div -> L.sdiv; 
              Equal -> L.icmp IP.EQ; Neq -> L.icmp IP.NE; 
              Less -> L.icmp IP.SLT; Leq -> L.icmp IP.SLE; 
              Greater -> L.icmp IP.SGT; Geq -> L.icmp IP.SGE;
              _ -> error "Internal error - semant failed") lhs' rhs'
codegenSexpr (TyFloat, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of Add -> L.fadd; Sub -> L.fsub; 
              Mult -> L.fmul; Div -> L.fdiv;
              Equal -> L.fcmp FP.OEQ; Neq -> L.fcmp FP.ONE; 
              Less -> L.fcmp FP.OLT; Leq -> L.fcmp FP.OLE; 
              Greater -> L.fcmp FP.OGT; Geq -> L.fcmp FP.OGE;
              _ -> error "Internal error - semant failed") lhs' rhs'
codegenSexpr (TyBool, SBinop op lhs rhs) = do
  lhs' <- codegenSexpr lhs
  rhs' <- codegenSexpr rhs
  (case op of And -> L.and; Or -> L.or; 
              _ -> error "Internal error - semant failed") lhs' rhs'

-- The Haskell LLVM bindings don't provide numerical or boolean negation
-- primitives, but they're easy enough to emit ourselves
codegenSexpr (TyInt, SUnop Neg e) = do 
  zero <- L.int32 0; e' <- codegenSexpr e; L.sub zero e'
codegenSexpr (TyFloat, SUnop Neg e) = do
  zero <- L.double 0; e' <- codegenSexpr e; L.fsub zero e'
codegenSexpr (TyBool, SUnop Not e) = do
  true <- L.bit 1; e' <- codegenSexpr e; L.xor true e'

codegenSexpr (_, SAssign name e) = do
  addr <- gets $ \vars -> vars M.! name
  e' <- codegenSexpr e
  L.store addr 0 e'
  return e'

codegenSexpr (_, SCall fun es) = do
  es' <- mapM (\e -> do e' <- codegenSexpr e; return (e', [])) es
  f <- gets $ \env -> env M.! fun
  L.call f es'

codegenSexpr (_, SNoexpr) = L.int32 0
-- Final catchall
codegenSexpr sx = 
  error $ "Internal error - semant failed. Invalid sexpr " ++ show sx

codegenStatement :: (MonadState Env m, L.MonadIRBuilder m) => SStatement -> m ()
codegenStatement (SExpr e) = void $ codegenSexpr e
codegenStatement (SReturn e) = codegenSexpr e >>= L.ret

codegenStatement (SBlock ss) = mapM_ codegenStatement ss

codegenStatement _ = error "If, for, and while WIP"

-- | Generate a function and add both the function name and variable names to
-- the map
codegenFunc :: SFunction -> LLVM ()
codegenFunc f = do
  let name = mkName (cs $ sname f)
      mkParam (t, n) = (ltypeOfTyp t, L.ParameterName (cs n))
      args = map mkParam (sformals f ++ slocals f)
      retty = ltypeOfTyp (styp f)
      body _ = do
        _entry <- L.block `L.named` "entry"
        forM_ (sformals f ++ slocals f) $ \(t, n) -> do
          let ltype = ltypeOfTyp t
          addr <- L.alloca ltype Nothing 0
          L.store addr 0 (AST.LocalReference ltype (mkName $ cs n))
          modify $ M.insert n addr
        mapM_ codegenStatement (sbody f)
  fun <- L.function name args retty body
  modify $ M.insert (sname f) fun

emitBuiltIns :: LLVM ()
emitBuiltIns = mapM_ emitBuiltIn (convert Semant.builtIns)
  where
    convert = map snd . M.toList
    emitBuiltIn f = 
      let fname = mkName (cs $ name f)
          paramTypes = map (ltypeOfTyp . fst) (formals f)
          retType = ltypeOfTyp (typ f)
      in do
        fun <- L.extern fname paramTypes retType
        modify $ M.insert (name f) fun

-- | codegenGlobal closely follows the structure of @extern defined in 
-- LLVM.IRBuilder.Module
codegenGlobal :: Bind -> LLVM ()
codegenGlobal (t, n) = do
  let name = mkName $ cs n
      typ  = ltypeOfTyp t
      var = O.ConstantOperand $ C.GlobalReference (AST.ptr typ) name
  L.emitDefn $ AST.GlobalDefinition G.globalVariableDefaults
   { G.name = name
   , G.type' = typ
   , G.linkage = Weak
   , G.initializer = Just $ C.Int 0 0
   }
  modify $ M.insert n var

codegenProgram :: SProgram -> AST.Module
codegenProgram (globals, funcs) = 
  flip evalState M.empty $ L.buildModuleT "microc" $ do
    emitBuiltIns 
    mapM_ codegenGlobal globals
    mapM_ codegenFunc funcs
