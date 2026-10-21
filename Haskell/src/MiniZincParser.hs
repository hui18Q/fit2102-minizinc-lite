module MiniZincParser
    ( miniZincParser
    ) where

import Control.Applicative (many, some, (<|>))
import Data.Char (isAlphaNum)
import Data.List (nub)
import Instances (ParseError (UnexpectedString), Parser)
import Parser (alpha, digit, failed, satisfy, sepBy1, stringTok, tok)

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
-- Domains and declarations
--------------------------------------------------------------------------------

-- Environment of previously declared constants.
type Constants = [(Identifier, Int)]

-- Parses an optional leading minus sign.
signed :: Parser Int -> Parser Int
signed p = negate <$> (symbol "-" *> p) <|> p

-- Parses a signed integer literal.
signedInteger :: Parser Int
signedInteger = signed natural

-- Looks up a previously declared constant.
constantValue :: Constants -> Parser Int
constantValue constants = do
    name <- identifier
    case lookup name constants of
        Just value -> pure value
        Nothing -> failed (UnexpectedString name)

-- Parses and resolves an integer domain bound.
domainBound :: Constants -> Parser Int
domainBound constants =
    signed (natural <|> constantValue constants)

-- Checks whether a list contains duplicate values.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- Parses an inclusive integer range.
rangeParser :: Constants -> Parser Domain
rangeParser constants = do
    lo <- domainBound constants
    _ <- symbol ".."
    hi <- domainBound constants
    if lo <= hi
        then pure (Range lo hi)
        else failed (UnexpectedString "Invalid range")

-- Parses a finite set containing distinct integers.
finiteSetParser :: Parser Domain
finiteSetParser = do
    _ <- symbol "{"
    values <- signedInteger `sepBy1` symbol ","
    _ <- symbol "}"
    if hasDuplicates values
        then failed (UnexpectedString "Duplicate set values")
        else pure (FiniteSet values)

-- Parses either a range or a finite set.
domainParser :: Constants -> Parser Domain
domainParser constants =
    rangeParser constants <|> finiteSetParser

-- Parses an integer constant declaration, currently supports signed integer literals only.
constantDeclaration :: Parser Declaration
constantDeclaration =
    ConstantDeclaration
        <$> (keyword "int" *> symbol ":" *> identifier)
        <*> (symbol "=" *>
             (IntLit <$> signedInteger) <* symbol ";")

-- Parses a variable declaration with its domain.
variableDeclaration :: Constants -> Parser Declaration
variableDeclaration constants = do
    _ <- keyword "var"
    dom <- domainParser constants
    _ <- symbol ":"
    name <- identifier
    _ <- symbol ";"
    pure (VariableDeclaration name dom)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")
