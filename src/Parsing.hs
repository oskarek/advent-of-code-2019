{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import           Text.Megaparsec         hiding ( ParseError )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Control.Monad                  ( void )

type ParseError = ParseErrorBundle Text Void
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed sc int

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

comma :: Parser ()
comma = void (symbol ",")

parse :: Parser a -> Text -> Either ParseError a
parse p = Text.Megaparsec.parse p ""

printParseError :: ParseError -> String
printParseError = Text.Megaparsec.errorBundlePretty
