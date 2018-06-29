module Parser where

import Ast
import Scanner
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Data.Either (lefts, rights)

opTable :: [[Operator Parser Expr]]
opTable = [ [unary Neg "-", unary Not "!"]
          , [infixL Mult "*", infixL Div "/"]
          , [infixL Add  "+", infixL Sub "-"]
          , [infixL Less "<", infixL Greater ">", 
             infixL Leq "<=", infixL Geq ">="]
          , [infixL Equal "==", infixL Neq "!="]
          , [InfixR $ (\lhs rhs -> case lhs of Id s -> Assign s rhs) <$ symbol "="]
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
  formals <- formalsP
  body' <- brackets $ many vdeclOrStatement
  let locals = lefts body'
      body = rights body'
  return $ Function name typ formals locals body

vdeclOrStatement :: Parser (Either Bind Statement)
vdeclOrStatement = (Left <$> try vdeclP) <|> (Right <$> statementP)

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` (symbol ",")
  where formalP = (,) <$> typeP <*> identifier

termP :: Parser Expr
termP = parens exprP
    <|> Literal <$> int
    <|> Fliteral <$> float
    <|> (BoolLit <$> ((rword "true" >> return True) <|> (rword "false" >> return False)))
    <|> Id <$> identifier
    <|> Call <$> identifier <*> parens (exprP `sepBy` (symbol ","))

exprP :: Parser Expr
exprP = makeExprParser termP opTable

statementP :: Parser Statement
statementP = (Expr <$> exprP <* semi)
   <|> (Return <$> (rword "return" *> exprP <* semi))
 -- hold off on control flow for now

programP :: Parser Program
programP = sc >> ((,) <$> try (many vdeclP) <*> many fdeclP <* eof
      <|>  (,) <$> return [] <*> many fdeclP <* eof)

