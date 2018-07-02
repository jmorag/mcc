module Parser (programP) where

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
  , [infixL Leq "<=", infixL Geq ">=", infixL Less "<", infixL Greater ">"]
  , [infixL Equal "==", infixL Neq "!="]
  , [infixL And "&&"], [infixL Or "||"]
  -- irrefutable pattern Id string on lhs of assignment
  , [InfixR $ (\(Id s) rhs -> Assign s rhs) <$ symbol "="] 
  ]
  where -- Megaparsec doesn't support multiple prefix operators by default,
        -- but we need this in order to parse things like double negatives or
        -- nots. Also, should we extend the language to include pointers, then 
        -- the * and ** operators become actually important.
        unary  op sym = Prefix $ foldr1 (.) <$> some (Unop op <$ symbol sym)
        infixL op sym = InfixL $ Binop op <$ symbol sym
        infixR op sym = InfixR $ Binop op <$ symbol sym

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

statementP :: Parser Statement
statementP = do
  block <- some statementP'
  case block of
    [single] -> return single
    block -> return (Block block)

statementP' :: Parser Statement
statementP' = Expr <$> exprP <* semi
  <|> Return <$> (rword "return" *> exprMaybe <* semi)
  <|> ifP <|> forP <|> whileP <|> brackets statementP
  where
    ifP = If <$> (rword "if" *> parens exprP) <*> statementP 
             <*> option (Block []) (rword "else" *> statementP)
    forP = For <$> (rword "for" *> symbol "(" *> exprMaybe <* semi) 
               <*> (exprP <* semi) <*> (exprMaybe <* symbol ")") 
               <*> statementP
    whileP = While <$> (rword "while" *> parens exprP) <*> statementP
    exprMaybe = option Noexpr exprP

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

