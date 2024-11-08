{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Inner (module Lexer.Inner) where

import Data.Text (Text)
import Data.Void (Void)
import Lexer.TokenTypes (Token (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
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

-- parse all tokens until EOF, then append an EOF token
-- creates a parser that generates a list of [Token] from Text
lexer :: Parser [Token]
lexer = do
  toks <- skip *> many (choice [pBinOp, pInt, pSymbol, pStr, pRWords, pIdent]) <* eof
  pure $ toks ++ [TEOF]

-- parse a signed integer
pInt :: Parser Token
pInt = TInt <$> lexeme (L.decimal <* notFollowedBy letterChar)

-- parse all characters between a pair of double quotes
-- allows strings to span multiple lines (includes '\n' character)
pStr :: Parser Token
pStr = TString <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

-- parse a binary operator
-- parse longest -> shortest operator symbols, to avoid early matching (e.g. "==" matching on first "=")
pBinOp :: Parser Token
pBinOp =
  TBinOp
    <$> ( stringLex ">=" -- ge
            <|> stringLex "<=" -- le
            <|> stringLex ">" -- gt
            <|> stringLex "<" -- lt
            <|> stringLex "==" -- eq
            <|> stringLex "!=" -- neq
            <|> stringLex "+" -- add
            <|> stringLex "-" -- subtract
            <|> stringLex "*" -- multiply
            <|> stringLex "/" -- divide
            <|> stringLex "=" -- assign
            <|> stringLex "&" -- logical AND
            <|> stringLex "|" -- logical OR
        )

-- parse a reserved word
pRWords :: Parser Token
pRWords = choice $ map pRWord rwords
  where
    pRWord :: Text -> Parser Token
    pRWord word = TRWord <$> (lexeme . try) (string word <* notFollowedBy alphaNumChar)
    rwords :: [Text]
    rwords =
      [ -- types
        "int",
        "bool",
        "str",
        -- modifiers
        "const",
        -- literals
        "true",
        "false",
        "null",
        -- control flow
        "if",
        "else",
        "while",
        -- builtins
        "print"
      ]

pIdent :: Parser Token
pIdent = TIdent <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

pSymbol :: Parser Token
pSymbol = choice [pSemi, pComma, pLParen, pRParen, pLBrace, pRBrace]
  where
    pSemi = TSemi <$ stringLex ";"
    pComma = TComma <$ stringLex ","
    pLParen = TLParen <$ stringLex "("
    pRParen = TRParen <$ stringLex ")"
    pLBrace = TLBrace <$ stringLex "{"
    pRBrace = TRBrace <$ stringLex "}"
