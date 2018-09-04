module Microc.Parser (programP, runParser, parseTest') where

import Microc.Ast
import Microc.Scanner
import Text.Megaparsec
import Text.Megaparsec.Expr

-- liftA2 f x y   = f <$> x <*> y
-- liftA3 f x y z = f <$> x <*> y <*> z
-- using these lifting functions creates less operator noise than writing out
-- <*> and <$> everywhere
import Control.Applicative (liftA2, liftA3)

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
    <|> BoolLit <$> (True <$ rword "true" <|> False <$ rword "false")
    <|> try (Call <$> identifier <*> parens (exprP `sepBy` comma))
    <|> Id <$> identifier

exprP :: Parser Expr
exprP = makeExprParser termP opTable

typeP :: Parser Type
typeP = TyInt   <$ rword "int"
    <|> TyBool  <$ rword "bool"
    <|> TyFloat <$ rword "float"
    <|> TyVoid  <$ rword "void"

vdeclP :: Parser Bind
vdeclP = Bind <$> typeP <*> identifier <* semi

statementP :: Parser Statement
statementP = Expr   <$> exprP <* semi
         <|> Return <$> (rword "return" *> exprMaybe <* semi)
         <|> Block  <$> brackets (many statementP)
         <|> ifP 
         <|> forP 
         <|> whileP

exprMaybe :: Parser Expr
exprMaybe = option Noexpr exprP

ifP :: Parser Statement
ifP = liftA3 If (rword "if" *> parens exprP) statementP maybeElse
  where
    maybeElse = option (Block []) (rword "else" *> statementP)
    
forP :: Parser Statement
forP = do
  rword "for"
  (e1, e2, e3) <- parens $ 
    liftA3 (,,) (exprMaybe <* semi) (exprP <* semi) exprMaybe
  For e1 e2 e3 <$> statementP

whileP :: Parser Statement
whileP = liftA2 While (rword "while" *> parens exprP) statementP

fdeclP :: Parser Function
fdeclP = Function <$> 
  typeP <*> identifier <*> formalsP <*> 
  (symbol "{" *> many vdeclP) <*> (many statementP <* symbol "}")

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` comma
  where formalP = liftA2 Bind typeP identifier

programP :: Parser Program
programP = between sc eof $ liftA2 Program (many $ try vdeclP) (many fdeclP)
