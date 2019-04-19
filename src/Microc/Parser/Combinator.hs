module Microc.Parser.Combinator
  ( programP
  , runParser
  , errorBundlePretty
  )
where

import           Microc.Ast
import           Microc.Scanner.Combinator
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Data.Either

opTable :: [[Operator Parser Expr]]
opTable =
  [ [InfixL $ Access <$ symbol "."]
  , [unary Neg "-", unary Not "!", unary Deref "*", unary Addr "&"]
  , [infixR Power "**"]
  , [infixL Mult "*", infixL Div "/"]
  , [infixL Add "+", infixL Sub "-"]
  , [infixL Leq "<=", infixL Geq ">=", infixL Less "<", infixL Greater ">"]
  , [infixL' Equal "==", infixL Neq "!="]
  , [infixL' BitAnd "&"]
  , [infixL' BitOr "|"]
  , [infixL' And "&&"]
  , [infixL' Or "||"]
  , [infixR Assign "="]
  ]
 where
  -- Megaparsec doesn't support multiple prefix operators by default,
  -- but we need this in order to parse things like double negatives,
  -- nots, and dereferences
  unary op sym = Prefix $ foldr1 (.) <$> some (Unop op <$ symbol sym)
  infixL op sym = InfixL $ Binop op <$ symbol sym
  -- Primed infixL' is useful for operators which are prefixes of other operators
  infixL' op sym = InfixL $ Binop op <$ operator sym
  infixR op sym = InfixR $ Binop op <$ symbol sym
  operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
  opChar = oneOf chars where chars = "!#$%&*+./<=>?@\\^|-~" :: String


termP :: Parser Expr
termP = try (Cast <$> parens typeP <*> exprP)
    <|> parens exprP
    <|> try (Fliteral <$> float)
    <|> Literal <$> int
    <|> BoolLit <$> (True <$ rword "true" <|> False <$ rword "false")
    <|> try (Call <$> identifier <*> parens (exprP `sepBy` comma))
    <|> Id <$> identifier

exprP :: Parser Expr
exprP = makeExprParser termP opTable

structP :: Parser Struct
structP = Struct <$> (rword "struct" *> identifier) <*> braces (many vdeclP) <* semi

typeP :: Parser Type
typeP = do
  baseType <- TyInt    <$ rword "int"
          <|> TyBool   <$ rword "bool"
          <|> TyFloat  <$ rword "float"
          <|> TyVoid   <$ rword "void"
          <|> TyStruct <$> (rword "struct" *> identifier)
  foldr (const Pointer) baseType <$> many star

vdeclP :: Parser Bind
vdeclP = Bind <$> typeP <*> identifier <* semi

statementP :: Parser Statement
statementP = Expr <$> exprP <*  semi
    <|> Return <$> (rword "return" *> exprMaybe <* semi)
    <|> Block  <$> braces (many statementP)
    <|> ifP
    <|> forP
    <|> whileP

exprMaybe :: Parser Expr
exprMaybe = option Noexpr exprP

ifP :: Parser Statement
ifP = liftA3 If (rword "if" *> parens exprP) statementP maybeElse
  where maybeElse = option (Block []) (rword "else" *> statementP)

forP :: Parser Statement
forP = do
  rword "for"
  (e1, e2, e3) <- parens
    $ liftA3 (,,) (exprMaybe <* semi) (exprP <* semi) exprMaybe
  For e1 e2 e3 <$> statementP

whileP :: Parser Statement
whileP = liftA2 While (rword "while" *> parens exprP) statementP

fdeclP :: Parser Function
fdeclP = Function <$> typeP <*> identifier <*> formalsP
    <*> (symbol "{" *> many vdeclP)
    <*> (many statementP <* symbol "}")

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` comma
  where formalP = liftA2 Bind typeP identifier

programP :: Parser Program
programP = between sc eof $ do
  structsOrGlobals <- many $ (try $ Left <$> structP) <|> (Right <$> try vdeclP)
  let structs = lefts structsOrGlobals
      globals = rights structsOrGlobals
  Program structs globals <$> many fdeclP
