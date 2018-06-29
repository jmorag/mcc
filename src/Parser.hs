module Parser where

import Ast
import Scanner
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Data.Either (lefts, rights)

opTable :: [[Operator Parser Expr]]
opTable = 
  [ [unary Neg "-", unary Not "!"]
  , [infixL Mult "*", infixL Div "/"]
  , [infixL Add  "+", infixL Sub "-"]
  , [infixL Leq "<=", infixL Geq ">=" ,infixL Less "<", infixL Greater ">"]
  , [infixL Equal "==", infixL Neq "!="]
  , [infixL And "&&"], [infixL Or "||"]
  , [InfixR $ (\lhs rhs -> case lhs of Id s -> Assign s rhs) <$ symbol "="]
  ]
  where unary op sym = Prefix $ Unop op <$ symbol sym
        infixL op sym = InfixL $ Binop op <$ symbol sym
        infixR op sym = InfixR $ Binop op <$ symbol sym

termP :: Parser Expr
termP = parens exprP
    <|> Literal <$> int
    <|> Fliteral <$> float
    <|> (BoolLit <$> ((rword "true" >> return True) <|> (rword "false" >> return False)))
    <|> try (Call <$> identifier <*> parens (exprP `sepBy` comma))
    <|> Id <$> identifier

exprP :: Parser Expr
exprP = makeExprParser termP opTable

typeP :: Parser Type
typeP = (rword "int"   >> return TyInt)
    <|> (rword "bool"  >> return TyBool)
    <|> (rword "float" >> return TyFloat)
    <|> (rword "void"  >> return TyVoid)

vdeclP :: Parser Bind
vdeclP = (,) <$> typeP <*> identifier <* semi

statementP :: Parser Statement
statementP = do
  block <- some statementP'
  case block of
    [single] -> return single
    block -> return (Block block)

statementP' :: Parser Statement
statementP' = Expr <$> exprP <* semi
  <|> Return <$> (rword "return" *> (option Noexpr exprP) <* semi)
  -- control flow WIP

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
vdeclOrStatement = Left <$> vdeclP <|> Right <$> statementP

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` comma
  where formalP = (,) <$> typeP <*> identifier

programP :: Parser Program
programP = sc >> ((,) <$> many (try vdeclP) <*> many fdeclP <* eof
      <|> (,) <$> return [] <*> many fdeclP <* eof)

