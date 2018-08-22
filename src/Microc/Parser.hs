module Microc.Parser (programP, runParser, parseTest') where

import Data.Text (Text)
import Microc.Ast
import Microc.Scanner
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Data.Either (lefts, rights)
import Control.Monad (void)

opTable :: [[Operator Parser Expr]]
opTable = 
  [ [unary Neg "-", unary Not "!"]
  , [infixL Mult "*", infixL Div "/"]
  , [infixL Add  "+", infixL Sub "-"]
  , [infixL Leq "<=", infixL Geq ">=", infixL Less "<", infixL Greater ">"]
  , [infixL Equal "==", infixL Neq "!="]
  , [infixL And "&&"], [infixL Or "||"]
  , [InfixR $ (\lhs rhs -> case lhs of 
      Id s -> Assign s rhs;
      _ -> error $ "Cannot assign to expression " <> show lhs) <$ symbol "="] 
  ]
  where -- Megaparsec doesn't support multiple prefix operators by default,
        -- but we need this in order to parse things like double negatives or
        -- nots. Also, should we extend the language to include pointers, then 
        -- the * and ** operators become actually important.
        unary  op sym = Prefix $ foldr1 (.) <$> some (Unop op <$ symbol sym)
        infixL op sym = InfixL $ Binop op <$ symbol sym

termP :: Parser Expr
termP = parens exprP
    <|> try (Fliteral <$> float)
    <|> Literal <$> int
    <|> (BoolLit <$> ((rword "true" >> return True) 
                 <|> (rword "false" >> return False)))
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

-- Parses a single statement, not a block
statementP :: Parser Statement
statementP = 
      Expr <$> exprP <* semi
  <|> Return <$> (rword "return" *> exprMaybe <* semi)
  <|> Block <$> brackets (many statementP)
  <|> ifP 
  <|> forP 
  <|> whileP

exprMaybe :: Parser Expr
exprMaybe = option Noexpr exprP


ifP :: Parser Statement
ifP = try withElse <|> withoutElse where
  withElse = do
    rword "if"
    cond <- parens exprP
    then' <- statementP
    rword "else"
    else' <- statementP
    return $ If cond then' else'
  withoutElse = do
    rword "if"
    cond <- parens exprP
    then' <- statementP
    return $ If cond then' (Block [])
    
forP :: Parser Statement
forP = do
  rword "for"
  void $ symbol "("
  e1 <- exprMaybe
  void semi
  e2 <- exprP
  void semi
  e3 <- exprMaybe
  void $ symbol ")"
  body <- statementP
  return $ For e1 e2 e3 body

whileP :: Parser Statement
whileP = While <$> (rword "while" *> parens exprP) <*> statementP

fdeclP :: Parser Function
fdeclP = do
  typ <- typeP
  name <- identifier
  formals <- formalsP
  body' <- brackets $ many vdeclOrStatement
  let locals = lefts body'
      body = rights body'
  return $ Function typ name formals locals body

vdeclOrStatement :: Parser (Either Bind Statement)
vdeclOrStatement = Left <$> vdeclP <|> Right <$> statementP

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` comma
  where formalP = (,) <$> typeP <*> identifier

programP :: Parser Program
programP = sc >> ((,) <$> many (try vdeclP) <*> many fdeclP <* eof
      <|> (,) <$> return [] <*> many fdeclP <* eof)

