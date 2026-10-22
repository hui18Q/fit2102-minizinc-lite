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

-- Parses a constant declaration containing an expression.
constantDeclaration :: Parser Declaration
constantDeclaration =
    ConstantDeclaration
        <$> (keyword "int" *> symbol ":" *> identifier)
        <*> (symbol "=" *> expressionParser <* symbol ";")

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
-- Arithmetic expressions
--------------------------------------------------------------------------------

-- Parses an arithmetic expression.
expressionParser :: Parser Expr
expressionParser = additionParser

-- Parses addition and subtraction from left to right.
additionParser :: Parser Expr
additionParser =
    chainLeft multiplicationParser additionOperator

-- Parses multiplication and division from left to right.
multiplicationParser :: Parser Expr
multiplicationParser =
    chainLeft unaryParser multiplicationOperator

-- Parses prefix negation.
unaryParser :: Parser Expr
unaryParser =
    (Neg <$> (symbol "-" *> unaryParser))
        <|> powerParser

-- Parses powers with right associativity.
powerParser :: Parser Expr
powerParser = do
    base <- atomParser
    (Pow base <$> (symbol "^" *> unaryParser))
        <|> pure base

-- Parses a literal, variable, or parenthesised expression.
atomParser :: Parser Expr
atomParser =
    numericMacroParser
        <|> (IntLit <$> natural)
        <|> (Var <$> identifier)
        <|> parenthesised expressionParser

-- Parses the addition and subtraction operators.
additionOperator :: Parser (Expr -> Expr -> Expr)
additionOperator =
    (Add <$ symbol "+")
        <|> (Sub <$ symbol "-")

-- Parses the multiplication and division operators.
multiplicationOperator :: Parser (Expr -> Expr -> Expr)
multiplicationOperator =
    (Mul <$ symbol "*")
        <|> (Div <$ symbol "/")

-- Parses terms and combines them left-associatively.
chainLeft
    :: Parser a
    -> Parser (a -> a -> a)
    -> Parser a
chainLeft term operator = foldl apply <$> term <*> many ((,) <$> operator <*> term)
  where
    apply acc (combine, right) = combine acc right

--------------------------------------------------------------------------------
-- Comparison operators and constraints
--------------------------------------------------------------------------------

-- Parses a MiniZinc comparison operator, longer operators are tried first so "<=" is not read as "<".
comparisonOperator :: Parser CompareOperator
comparisonOperator =
        (OpNe <$ symbol "!=")
    <|> (OpLe <$ symbol "<=")
    <|> (OpGe <$ symbol ">=")
    <|> (OpEq <$ symbol "=")
    <|> (OpLt <$ symbol "<")
    <|> (OpGt <$ symbol ">")

-- Parses two arithmetic expressions joined by a comparison, flip reorders the arguments: source is "lhs op rhs", but the constructor is Compare op lhs rhs.
comparisonParser :: Parser Constraint
comparisonParser =
    flip Compare
        <$> expressionParser
        <*> comparisonOperator
        <*> expressionParser

-- Parses either a boolean macro or a comparison.
constraintBody :: Parser Constraint
constraintBody =
    booleanMacroParser <|> comparisonParser

-- Parses a complete constraint statement.
constraintParser :: Parser Constraint
constraintParser =
    keyword "constraint"
        *> constraintBody
        <* symbol ";"

--------------------------------------------------------------------------------
-- Macro parsers
--------------------------------------------------------------------------------

-- Parses p surrounded by parentheses.
parenthesised :: Parser a -> Parser a
parenthesised p = symbol "(" *> p <* symbol ")"

-- Parses a non-empty list inside square brackets.
listParser :: Parser a -> Parser [a]
listParser item =
    symbol "["
        *> (item `sepBy1` symbol ",")
        <* symbol "]"

-- Parses the input of a macro (literal list form).
macroInput :: Parser a -> Parser (MacroInput a)
macroInput item = Explicit <$> listParser item

-- Parses the parenthesised input of a macro call.
-- Part D extends this for array and generator forms.
macroArgs :: Parser a -> Parser (MacroInput a)
macroArgs item = parenthesised (macroInput item)

-- Parses a named macro call: name(input).
macroCall :: String -> Parser a -> Parser (MacroInput a)
macroCall name item = keyword name *> macroArgs item

-- Parses the name of a numeric macro.
numericMacroName :: Parser NumericMacro
numericMacroName =
        (Sum <$ keyword "sum")
    <|> (Minimum <$ keyword "min")
    <|> (Maximum <$ keyword "max")

-- Parses a numeric macro as an expression.
numericMacroParser :: Parser Expr
numericMacroParser =
    NumericMacroExpr <$> numericMacroName <*> macroArgs expressionParser

-- Parses all, any and allDifferent constraints.
-- Order is irrelevant: keyword checks the whole word.
booleanMacroParser :: Parser Constraint
booleanMacroParser =
        (All <$> macroCall "all" comparisonParser)
    <|> (Any <$> macroCall "any" comparisonParser)
    <|> (AllDifferent <$> macroCall "allDifferent" expressionParser)

--------------------------------------------------------------------------------
-- Solve directives
--------------------------------------------------------------------------------

-- Parses one of the three supported solve goals.
solveGoalParser :: Parser SolverGoal
solveGoalParser =
        (Satisfy <$ keyword "satisfy")
    <|> (Minimize <$> (keyword "minimize" *> expressionParser))
    <|> (Maximize <$> (keyword "maximize" *> expressionParser))

-- Parses a complete solve directive.
solveDirectiveParser :: Parser SolverGoal
solveDirectiveParser =
    keyword "solve"
        *> solveGoalParser
        <* symbol ";"

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")
