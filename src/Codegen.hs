module Codegen where

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST as AST
import qualified LLVM.AST.Operand as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Typed as AST
import LLVM.AST.Name

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L

import qualified Data.Map as M
import Control.Monad.State
import Data.String (fromString)

import Sast
import Ast (Type(..))
import Ast (Op(..))
import Ast (Uop(..))

-- When using the IRBuilder, both functions and variables have the type Operand
type Env = M.Map String AST.Operand
-- Codegen Type has to be more sophisticated to be used for both IRBuilder and
-- ModuleBuilder functions
type Codegen = L.IRBuilderT (State Env)

ltypeOfTyp :: Type -> AST.Type
ltypeOfTyp TyVoid = AST.void
ltypeOfTyp TyInt = AST.i32
ltypeOfTyp TyFloat = AST.double
ltypeOfTyp TyBool = AST.IntegerType 1


codegenSexpr :: SExpr -> Codegen AST.Operand

codegenSexpr (TyInt, SLiteral i) = L.int32 (fromIntegral i)
codegenSexpr (TyFloat, SFliteral f) = L.double f
codegenSexpr (TyBool, SBoolLit b) = L.bit (if b then 1 else 0)
codegenSexpr (ty, SId name) = do
  vars <- get
  case M.lookup name vars of
    Just addr -> L.load addr 0
    Nothing -> error $ "Internal error - undefined variable name " ++ name 

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
  vars <- get
  case M.lookup name vars of
    Just addr -> do
      e' <- codegenSexpr e
      L.store addr 0 e'
      -- We return the evaluated expression on the rhs of the assignment
      return e'
    Nothing -> error $ "Internal error - invalid variable name " ++ name

codegenSexpr (_, SCall fun es) = do
  funcs <- get
  case M.lookup fun funcs of
    Just f -> do
      es' <- mapM codegenSexpr es
      -- L.call expects a list of function attributes attached to each argument
      -- Since we don't do any fancy LLVM shenanigans here, we leave them blank
      L.call f $ map (\e -> (e, [])) es'
    Nothing -> error $ "Internal error - undefined function " ++ fun

codegenSexpr (_, SNoexpr) = L.int32 0
-- Final catchall
codegenSexpr sx = 
  error $ "Internal error - semant failed. Invalid sexpr " ++ show sx

-- codegen for statements should not return anything, so it just executes "side
-- effects" in the IRBuilder monad to advance the builder's position
codegenStatement :: SStatement -> Codegen ()
codegenStatement (SExpr e) = codegenSexpr e >> return ()
codegenStatement (SReturn e) = codegenSexpr e >>= L.ret
codegenStatement s = error $ "Blocks, if, for, and while WIP"

-- codegenFunc :: SFunction -> Codegen ()
codegenFunc f = 
  let name = mkName (sname f)
      mkParam (t, n) = (ltypeOfTyp t, L.ParameterName (fromString n))
      args = map mkParam (sformals f)
      retty = ltypeOfTyp (styp f)
      -- generate code to allocate space for formals, locals, and generate code
      -- for the statments
      body :: [AST.Operand] -> Codegen ()
      body formals = do
        forM_ formals $ \f@(AST.LocalReference t (Name name)) -> do
          alloc <- L.alloca t (Just f) 0
          modify $ \env -> M.insert (show name) alloc env
        
  in L.function name args retty body
