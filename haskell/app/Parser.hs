{-# LANGUAGE OverloadedStrings #-}

module Parser (Parser, pExpr) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Types (Expr (..))

-- parser types

type Parser = Parsec Void Text

-- generic parsers

-- what text should be skipped?
-- whitespace, inline comments, and block comments
skip :: Parser ()
skip = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "###" "###")

-- only succeed parsing if the parsed input meets a boolean check
-- inputs: function check on parsed value -> error msg -> parser to run
-- return: parser that performs a check on the parsed value
withPredicate :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate f msg p = do
  offset <- getOffset
  result <- p
  if f result
    then pure result
    else do
      setOffset offset
      fail msg

-- remove trailing whitespace after applying the argument parser
lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme skip

-- parse specified Text then consume trailing whitespace
stringLex :: Text -> Parser Text
stringLex = L.symbol skip

-- parse an (unsigned) integer and remove trailing whitespace
int' :: Parser Int
int' = lexeme' L.decimal

-- parse a (unsigned) float and remove trailing whitespace
float' :: Parser Float
float' = lexeme' L.float

-- parse a (signed) integer and remove trailing whitespace
signedInt :: Parser Int
signedInt = L.signed (return ()) int'

-- parse a (signed) float and remove trailing whitespace
signedFloat :: Parser Float
signedFloat = L.signed (return ()) float'

-- parse a character surrounded by single quotes
-- e.g. 'a'
charLit :: Parser Char
charLit = between (char '\'') (char '\'') L.charLiteral

-- parse a character surrounded by single quotes
stringLit :: Parser String
stringLit = char '"' *> manyTill L.charLiteral (char '"')

-- =========================
--  coding-specific parsers
-- =========================

-- parse a variable identifier
pIdent :: Parser Expr
pIdent = label "identifier" $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ IDENT $ first : rest

pInt :: Parser Expr
pInt = INT <$> signedInt

pFloat :: Parser Expr
pFloat = FLOAT <$> signedFloat

-- parse inside a set of parentheses
pParens :: Parser a -> Parser a
pParens = between (char '(') (char ')')

pTerm :: Parser Expr
pTerm = lexeme' $ choice [pParens pExpr, pIdent, pInt]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- =========================
--  operators
-- =========================

-- helper function, creates a binary operator
-- op -> operator symbol (e.g. "+", "%")
-- f  -> function to apply the operator to
binary' :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary' op f = InfixL (f <$ stringLex op)

-- helper function, creates a unary operator
-- op -> operator symbol (e.g. "-", "+")
-- f  -> function to apply the operator to
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix op f = Prefix (f <$ stringLex op)
postfix op f = Postfix (f <$ stringLex op)

-- operators defined in order of precedence (highest -> lowest)
-- operators in the same inner list have equal precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" NEG, prefix "+" id],
    [binary' "*" PRODUCT, binary' "/" DIVISION, binary' "%" MODULUS],
    [binary' "+" SUM, binary' "-" DIFFERENCE]
  ]
