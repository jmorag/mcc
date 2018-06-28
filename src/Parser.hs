module Parser where

import Ast
import Scanner
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Control.Applicative
import Data.Either (lefts, rights)
import Data.List (intersperse)

opTable :: [[Operator Parser Expr]]
opTable = [ [unary Neg "-", unary Not "!"]
          , [infixL Mult "*", infixL Div "/"]
          , [infixL Add  "+", infixL Sub "-"]
          , [infixL Less "<", infixL Greater ">", 
             infixL Leq "<=", infixL Geq ">="]
          , [infixL Equal "==", infixL Neq "!="]
          , [InfixR (\lhs rhs -> case lhs of Id s -> Assign s rhs) <$ symbol "="]
          ]
  where unary op sym = Prefix $ Unop op <$ symbol sym
        infixL op sym = InfixL $ Binop op <$ symbol sym
        infixR op sym = InfixR $ Binop op <$ symbol sym


typeP :: Parser Type
typeP = (rword "int"   >> return TyInt)
    <|> (rword "bool"  >> return TyBool)
    <|> (rword "float" >> return TyFloat)
    <|> (rword "void"  >> return TyVoid)

vdeclP :: Parser Bind
vdeclP = do
  typ <- typeP
  name <- identifier
  semi
  return (typ, name)

fdeclP :: Parser Function
fdeclP = do
  typ <- typeP
  name <- identifier
  formals <- parens argsP
  body' <- brackets $ many vdeclOrStatement
  let locals = lefts body'
      body = rights body'
  return $ Function name typ formals locals body

vdeclOrStatement :: Parser (Either Bind Statement)
vdeclOrStatement = (Left <$> try vdeclP) <|> (Right <$> statementP)

argsP :: Parser [Bind]
argsP = intersperse <$> (symbol ",") <*> many argP
  where argP = (,) <$> typeP <*> identifier

termP :: Parser Expr
termP = parens exprP
    <|> Literal <$> int
    <|> FLiteral <$> float
    <|> (BoolLit <$> (try . rword "true" >> True <|> rword "false" >> False))
    <|> Id <$> identifier

exprP :: Parser Expr
exprP = makeExprParser termP opTable

statementP :: Parser Statement
statementP = exprP >>= return . Expr
 <|> many statementP >>= return . Block
 <|> rword "return" >> exprP >>= return . Return
 -- hold off on control flow for now

programP :: Parser Program
programP = (,) <$> many vdeclP <*> many fdeclP
