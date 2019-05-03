module Microc.Scanner.Combinator where

import           Data.Void
import           Data.Char
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad                  ( void )
import           Data.String.Conversions

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

star :: Parser ()
star = void $ symbol "*"

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text] -- list of reserved words
rws =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "true"
  , "false"
  , "for"
  , "int"
  , "bool"
  , "char"
  , "float"
  , "void"
  , "return"
  , "struct"
  , "NULL"
  , "sizeof"
  ]

strlit :: Parser Text
strlit = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  -- Hijack haskell's string lexer so we don't have to deal with escaping
  pure $ T.pack (read ('"' : cs content ++ "\""))

charlit :: Parser Int
charlit =
  squotes $ (ord <$> satisfy (`notElem` special)) <|> (single '\\' >> int)
 where
  special :: String
  special = "\\'"

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
 where
  p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
  check x = if x `elem` rws
    then fail $ "keyword " <> show x <> " cannot be an identifier"
    else return x

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float
