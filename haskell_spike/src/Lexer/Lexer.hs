{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Lexer (lexer) where

import Data.Text (Text)
import Data.Void (Void)
import Lexer.TokenTypes (Token (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
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
  toks <- skip *> many (choice [pBinOp, pInt, pSymbol, pRWords, pIdent]) <* eof
  pure $ toks ++ [TEOF]

-- parse a signed integer
pInt :: Parser Token
pInt = TInt <$> L.signed empty (lexeme (L.decimal <* notFollowedBy letterChar))

-- parse a binary operator
-- parse longest -> shortest operator symbols, to avoid early matching (e.g. "==" matching on first "=")
pBinOp :: Parser Token
pBinOp =
  TBinOp
    <$> ( stringLex ">=" -- ge
            <|> stringLex "<=" -- le
            <|> stringLex "==" -- eq
            <|> stringLex "!=" -- neq
            <|> stringLex "+" -- add
            <|> stringLex "-" -- subtract
            <|> stringLex "*" -- multiply
            <|> stringLex "/" -- divide
            <|> stringLex "%" -- modulus
            <|> stringLex "=" -- assign
            <|> stringLex "&" -- logical AND
            <|> stringLex "|" -- logical OR
            <|> stringLex ">" -- gt
            <|> stringLex "<" -- lt
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
pSymbol = choice [pSemi, pLParen, pRParen, pLBrace, pRBrace]
  where
    pSemi = TSemi <$ stringLex ";"
    pLParen = TLParen <$ stringLex "("
    pRParen = TRParen <$ stringLex ")"
    pLBrace = TLBrace <$ stringLex "{"
    pRBrace = TRBrace <$ stringLex "}"
