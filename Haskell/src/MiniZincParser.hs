module MiniZincParser
    ( miniZincParser
    ) where

import Control.Applicative (many, some)
import Data.Char (isAlphaNum)
import Instances (ParseError (UnexpectedString), Parser)
import Parser (failed, alpha, digit, satisfy, stringTok, tok)

import MiniZincADT

--------------------------------------------------------------------------------
-- Lexical parsers
--------------------------------------------------------------------------------

-- Parses a symbol and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = stringTok

-- Parses an identifier-shaped word.
rawIdentifier :: Parser String
rawIdentifier =
    tok ((:) <$> alpha <*> many (satisfy validPart))
  where
    validPart c = isAlphaNum c || c == '_'

-- Words reserved by the MiniZinc-Lite language.
reservedWords :: [String]
reservedWords =
    [ "var", "int", "constraint", "solve"
    , "array", "of", "in"
    , "satisfy", "maximize", "minimize"
    , "sum", "min", "max"
    , "all", "any", "allDifferent"
    ]

-- Parses an identifier that is not a reserved word.
identifier :: Parser String
identifier = do
    name <- rawIdentifier
    if name `elem` reservedWords
        then failed (UnexpectedString name)
        else pure name

-- Parses a keyword as a complete word.
keyword :: String -> Parser String
keyword expected = do
    actual <- rawIdentifier
    if actual == expected
        then pure actual
        else failed (UnexpectedString actual)

-- Parses one or more digits as a non-negative integer.
natural :: Parser Int
natural = tok (read <$> some digit)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")
