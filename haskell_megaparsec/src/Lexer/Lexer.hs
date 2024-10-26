{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Lexer (module Lexer.Lexer) where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators (choice)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types (Token (..))

-- parser type
type Parser = Parsec Void Text

-- ==================== SPACE CONSUMERS ====================

-- space consumer (text to be skipped)
-- whitespace, inline comments, and block comments
skip :: Parser ()
skip = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "###" "###")

-- apply argument parser then consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

-- expect & parse specified Text then consume trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol skip

-- ==================== DELIMITER / SYMBOL PARSERS ====================

pDQuotes, pSQuotes :: Parser a -> Parser a
pDQuotes = between (char '"') (char '"')
pSQuotes = between (char '\'') (char '\'')

pSemicolon, pColon, pComma :: Parser Token
pSemicolon = TSemicolon <$ char ';'
pColon = TColon <$ char ':'
pComma = TComma <$ char ','

-- identifiers may start with a letter or underscore
-- may contain numbers after the first character
pIdent :: Parser Token
pIdent = do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ TIdent $ first : rest

-- ==================== KEYWORD PARSERS ====================

pKeywords :: Parser Token
pKeywords = choice $ map pKeyword keywords
  where
    pKeyword :: Text -> Parser Token
    pKeyword word = TKeyword <$> (lexeme . try) (string word <* notFollowedBy alphaNumChar)

-- list of reserved keywords
keywords :: [Text]
keywords =
  [ -- literal values
    "true",
    "false",
    "null",
    -- types
    "char",
    "int",
    "float",
    "String",
    -- control flow
    "if",
    "elif",
    "else",
    "while",
    "for",
    -- statements
    "fn",
    "class",
    "return"
  ]

-- ==================== LITERAL PARSERS ====================

-- parse a (signed) numeric value
pNumLit :: Parser Token
pNumLit = try pFloat <|> pInt
  where
    pInt = TInt <$> L.signed (return ()) (lexeme L.decimal)
    pFloat = TFloat <$> L.signed (return ()) (lexeme L.float)

-- parse a character surrounded by single quotes (e.g. 'a')
pCharLit :: Parser Token
pCharLit = TCharLit <$> pSQuotes L.charLiteral

-- parse a string surrounded by double quotes (e.g. "myString")
pStringLit :: Parser Token
pStringLit = TStringLit <$> pDQuotes (takeWhileP Nothing (/= '"'))

-- parse brackets
pOpenParen, pCloseParen, pOpenBrace, pCloseBrace :: Parser Token
pOpenParen = TLParen <$ lexeme (char '(')
pCloseParen = TRParen <$ lexeme (char ')')
pOpenBrace = TLBrace <$ lexeme (char '{')
pCloseBrace = TRBrace <$ lexeme (char '}')

lex :: Parser [Token]
lex = many (lexeme $ choice [pNumLit, pOpenBrace, pCloseBrace, pOpenParen, pCloseParen, pKeywords, pIdent]) <* eof

-- -- parse inside a set of parentheses
-- pParens :: Parser a -> Parser a
-- pParens = between (char '(') (char ')')
--
-- pTerm :: Parser Expr
-- pTerm = lexeme $ choice [pParens pExpr, pIdent, pInt]
--
-- pExpr :: Parser Expr
-- pExpr = makeExprParser pTerm operatorTable
--
-- -- =========================
-- --  operators
-- -- =========================
--
-- -- helper function, creates a binary operator
-- -- op -> operator symbol (e.g. "+", "%")
-- -- f  -> function to apply the operator to
-- binary' :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
-- binary' op f = InfixL (f <$ symbol op)
--
-- -- helper function, creates a unary operator
-- -- op -> operator symbol (e.g. "-", "+")
-- -- f  -> function to apply the operator to
-- prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
-- prefix op f = Prefix (f <$ symbol op)
-- postfix op f = Postfix (f <$ symbol op)
--
-- -- operators defined in order of precedence (highest -> lowest)
-- -- operators in the same inner list have equal precedence
-- operatorTable :: [[Operator Parser Expr]]
-- operatorTable =
--   [ [prefix "-" NEG, prefix "+" id],
--     [binary' "*" PRODUCT, binary' "/" DIVISION, binary' "%" MODULUS],
--     [binary' "+" SUM, binary' "-" DIFFERENCE]
--   ]
--
-- import Types (Token (..))
--
-- -- parse a variable identifier
-- pIdent :: Parser Token
-- pIdent = label "identifier" $ do
--   first <- letterChar <|> char '_'
--   rest <- many $ alphaNumChar <|> char '_'
--   pure $ TIdent $ first : rest
--
-- pInt :: Parser Expr
-- pInt = INT <$> signedInt
--
-- pFloat :: Parser Expr
-- pFloat = FLOAT <$> signedFloat
--
-- -- parse inside a set of parentheses
-- pParens :: Parser a -> Parser a
-- pParens = between (char '(') (char ')')
--
-- pTerm :: Parser Expr
-- pTerm = lexeme $ choice [pParens pExpr, pIdent, pInt]
--
-- pExpr :: Parser Expr
-- pExpr = makeExprParser pTerm operatorTable
--
-- -- =========================
-- --  operators
-- -- =========================
--
-- -- helper function, creates a binary operator
-- -- op -> operator symbol (e.g. "+", "%")
-- -- f  -> function to apply the operator to
-- binary' :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
-- binary' op f = InfixL (f <$ symbol op)
--
-- -- helper function, creates a unary operator
-- -- op -> operator symbol (e.g. "-", "+")
-- -- f  -> function to apply the operator to
-- prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
-- prefix op f = Prefix (f <$ symbol op)
-- postfix op f = Postfix (f <$ symbol op)
--
-- -- operators defined in order of precedence (highest -> lowest)
-- -- operators in the same inner list have equal precedence
-- operatorTable :: [[Operator Parser Expr]]
-- operatorTable =
--   [ [prefix "-" NEG, prefix "+" id],
--     [binary' "*" PRODUCT, binary' "/" DIVISION, binary' "%" MODULUS],
--     [binary' "+" SUM, binary' "-" DIFFERENCE]
--   ]
