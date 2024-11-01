{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Lexer where

import Data.Text (Text)
import Data.Void (Void)
import Lexer.TokenTypes (Token (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- parser type that parses Text
type Parser = Parsec Void Text

-- ====================  SPACE CONSUMERS ====================

-- parse whitespace (spaces, line comments, no block comments)
skip :: Parser ()
skip = L.space space1 (L.skipLineComment "#") empty

-- modifies a parser to consume trailing whitespace after parsing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

-- parse a specific string of text, then consume trailing whitespace
stringLex :: Text -> Parser Text
stringLex = L.symbol skip

-- ====================  LITERAL PARSERS ====================

-- parse a signed integer
pInt :: Parser Token
pInt = TInt <$> L.signed empty (lexeme L.decimal)

pBinOp :: Parser Token
pBinOp =
  TBinOp
    <$> ( stringLex "+"
            <|> stringLex "-"
            <|> stringLex "*"
            <|> stringLex "/"
            <|> stringLex "%"
        )

pKeyword :: Parser Token
pKeyword = TKeyword <$> stringLex "print"

pSymbol :: Parser Token
pSymbol = pLParen <|> pRParen <|> pSemi
  where
    pLParen, pRParen, pSemi :: Parser Token
    pSemi = TSemi <$ stringLex ";"
    pLParen = TLParen <$ stringLex "("
    pRParen = TRParen <$ stringLex ")"

lexer :: Parser [Token]
lexer = many (choice [pBinOp, pInt, pSymbol, pKeyword]) <* eof
