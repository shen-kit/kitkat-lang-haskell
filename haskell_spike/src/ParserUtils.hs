{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserUtils (Parser, skip, lexeme, stringLex) where

import Data.Text (Text)
import Data.Void (Void)
import Lexer.TokenTypes (Token)
import Text.Megaparsec (Parsec, empty)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- ====================  SPACE CONSUMERS ====================

type Parser = Parsec Void Text

-- parse whitespace (spaces, line comments, no block comments)
skip :: Parser ()
skip = L.space space1 (L.skipLineComment "#") empty

-- modifies a parser to consume trailing whitespace after parsing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

-- parse a specific string of text, then consume trailing whitespace
stringLex :: Text -> Parser Text
stringLex = L.symbol skip
