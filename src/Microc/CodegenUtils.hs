module Microc.CodegenUtils where

-------------------------------------------------------------------------------
-- The following functions should really be included in llvm-hs. I'll open a PR
-- soon to have something like these included in the near future
-------------------------------------------------------------------------------
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import LLVM.AST.Linkage
import LLVM.AST.Name
import LLVM.AST.Typed (typeOf)

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import Control.Monad.State
import           Data.Char (ord)

-- | A global variable definition
global
  :: L.MonadModuleBuilder m
  => Name       -- ^ Variable name
  -> AST.Type   -- ^ Type
  -> C.Constant -- ^ Initializer
  -> m O.Operand
global nm ty initVal = do
  L.emitDefn $ AST.GlobalDefinition G.globalVariableDefaults
    { G.name        = nm
    , G.type'       = ty
    , G.linkage     = Weak
    , G.initializer = Just initVal
    }
  pure $ O.ConstantOperand $ C.GlobalReference (AST.ptr ty) nm

globalStringPtr :: L.MonadModuleBuilder m => String -> Name -> m O.Operand
globalStringPtr str name = do
  let asciiVals = map (fromIntegral . ord) str
      llvmVals  = map (C.Int 8) (asciiVals ++ [0])
      charArray = C.Array char llvmVals
      typ = typeOf charArray
      var = O.ConstantOperand $ C.BitCast (C.GlobalReference (AST.ptr typ) name) charStar
  L.emitDefn $ AST.GlobalDefinition G.globalVariableDefaults
   { G.name = name
   , G.type' = typ
   , G.linkage = Private
   , G.isConstant = True
   , G.initializer = Just charArray
   , G.unnamedAddr = Just G.GlobalAddr
   }
  pure var
  where
    char = AST.IntegerType 8
    charStar = AST.ptr $ AST.IntegerType 8


externVarArgs :: L.MonadModuleBuilder m => Name -> [AST.Type] -> AST.Type -> m O.Operand
externVarArgs nm argtys retty = do
  L.emitDefn $ AST.GlobalDefinition G.functionDefaults
    { G.name        = nm
    , G.linkage     = External
    , G.parameters  = ([G.Parameter ty (mkName "") [] | ty <- argtys], True)
    , G.returnType  = retty
    }
  let funty = AST.ptr $ AST.FunctionType retty argtys True
  pure $ O.ConstantOperand $ C.GlobalReference funty nm

-- | Check if the currently active block has a terminator
-- Note: this will generate an error if there is no currently active block
hasTerminator :: L.MonadIRBuilder m => m Bool
hasTerminator = do
  current <- L.liftIRState $ gets L.builderBlock
  case current of
    Nothing    -> error "No currently active block"
    Just block -> case L.partialBlockTerm block of
      Nothing -> return False
      Just _  -> return True
