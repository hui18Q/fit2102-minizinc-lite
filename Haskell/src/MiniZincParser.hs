module MiniZincParser
    ( miniZincParser
    ) where

import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, when)
import Data.Char (isAlphaNum)
import Instances (ParseError (UnexpectedString), Parser)
import Parser
    ( alpha, commaTok, digit, eof, failed, satisfy, sepBy1, spaces, stringTok
    , tok
    )

import MiniZincADT
import MiniZincEval (Env, evalExpr, hasDuplicates)

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
    , "mod", "abs"
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
type Constants = Env

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
    values <- signedInteger `sepBy1` commaTok
    _ <- symbol "}"
    if hasDuplicates values
        then failed (UnexpectedString "Duplicate set values")
        else pure (FiniteSet values)

-- Parses either a range or a finite set.
domainParser :: Constants -> Parser Domain
domainParser constants =
    rangeParser constants <|> finiteSetParser

-- Parses a constant declaration containing an expression.
constantDeclaration :: Constants -> Parser Declaration
constantDeclaration constants =
    ConstantDeclaration
        <$> (keyword "int" *> symbol ":" *> identifier)
        <*> (symbol "=" *> expressionParser constants <* symbol ";")

-- Parses "var <domain>: <identifier>;", shared by plain and array
-- declarations.
varBody :: Constants -> Parser (Domain, Identifier)
varBody constants =
    (,)
        <$> (keyword "var" *> domainParser constants)
        <*> (symbol ":" *> identifier <* symbol ";")

-- Parses a variable declaration with its domain.
variableDeclaration :: Constants -> Parser Declaration
variableDeclaration constants =
    (\(dom, name) -> VariableDeclaration name dom) <$> varBody constants

-- Parses "array[1..<length>] of", returning the length. The lower
-- bound must be the literal 1 and the length must be at least 1.
arrayLength :: Constants -> Parser Int
arrayLength constants = do
    lower <- keyword "array" *> symbol "[" *> natural
    len <- symbol ".." *> domainBound constants <* symbol "]" <* keyword "of"
    unless (lower == 1 && len >= 1)
        (failed (UnexpectedString "Invalid array bounds"))
    pure len

-- Parses an array declaration such as array[1..n] of var 1..n: q;
arrayDeclaration :: Constants -> Parser Declaration
arrayDeclaration constants = do
    len <- arrayLength constants
    (dom, name) <- varBody constants
    pure (ArrayDeclaration name len dom)

--------------------------------------------------------------------------------
-- Arithmetic expressions
--------------------------------------------------------------------------------

-- Parses an arithmetic expression. The declared constants are passed
-- down because a generator domain inside a macro, as in
-- sum(i in 1..n)(q[i]), is resolved to literal bounds while parsing.
expressionParser :: Constants -> Parser Expr
expressionParser = additionParser

-- Parses addition and subtraction from left to right.
additionParser :: Constants -> Parser Expr
additionParser constants =
    chainLeft (multiplicationParser constants) additionOperator

-- Parses multiplication and division from left to right.
multiplicationParser :: Constants -> Parser Expr
multiplicationParser constants =
    chainLeft (unaryParser constants) multiplicationOperator

-- Parses prefix negation.
unaryParser :: Constants -> Parser Expr
unaryParser constants =
    (Neg <$> (symbol "-" *> unaryParser constants))
        <|> powerParser constants

-- Parses powers with right associativity.
powerParser :: Constants -> Parser Expr
powerParser constants = do
    base <- atomParser constants
    (Pow base <$> (symbol "^" *> unaryParser constants))
        <|> pure base

-- Parses an indexed array element such as q[1] or q[i].
arrayAccessParser :: Constants -> Parser Expr
arrayAccessParser constants =
    ArrayAccess
        <$> identifier
        <*> (symbol "[" *> expressionParser constants <* symbol "]")

-- Parses abs(<expr>), the absolute value (Part E).
absParser :: Constants -> Parser Expr
absParser constants =
    Abs <$> (keyword "abs" *> parenthesised (expressionParser constants))

-- Parses a literal, array element, variable, or parenthesised
-- expression. q[...] is tried before a plain variable, otherwise
-- q would be read on its own and leave [...] unconsumed.
atomParser :: Constants -> Parser Expr
atomParser constants =
    numericMacroParser constants
        <|> absParser constants
        <|> (IntLit <$> natural)
        <|> arrayAccessParser constants
        <|> (Var <$> identifier)
        <|> parenthesised (expressionParser constants)

-- Parses the addition and subtraction operators.
additionOperator :: Parser (Expr -> Expr -> Expr)
additionOperator =
    (Add <$ symbol "+")
        <|> (Sub <$ symbol "-")

-- Parses the multiplication, division and mod operators, which share
-- one precedence level. mod is a keyword, so "model" is not read as it.
multiplicationOperator :: Parser (Expr -> Expr -> Expr)
multiplicationOperator =
    (Mul <$ symbol "*")
        <|> (Div <$ symbol "/")
        <|> (Mod <$ keyword "mod")

-- Parses terms and combines them left-associatively.
chainLeft
    :: Parser a
    -> Parser (a -> a -> a)
    -> Parser a
chainLeft term operator =
    foldl apply <$> term <*> many ((,) <$> operator <*> term)
  where
    apply acc (combine, right) = combine acc right

--------------------------------------------------------------------------------
-- Comparison operators and constraints
--------------------------------------------------------------------------------

-- Parses a MiniZinc comparison operator. Longer operators are tried
-- first so "<=" is not read as "<".
comparisonOperator :: Parser CompareOperator
comparisonOperator =
        (OpNe <$ symbol "!=")
    <|> (OpLe <$ symbol "<=")
    <|> (OpGe <$ symbol ">=")
    <|> (OpEq <$ symbol "=")
    <|> (OpLt <$ symbol "<")
    <|> (OpGt <$ symbol ">")

-- Parses two arithmetic expressions joined by a comparison. flip
-- reorders the arguments: the source is "lhs op rhs", but the
-- constructor is Compare op lhs rhs.
comparisonParser :: Constants -> Parser Constraint
comparisonParser constants =
    flip Compare
        <$> expressionParser constants
        <*> comparisonOperator
        <*> expressionParser constants

-- Parses either a boolean macro or a comparison.
constraintBody :: Constants -> Parser Constraint
constraintBody constants =
    booleanMacroParser constants <|> comparisonParser constants

-- Parses a complete constraint statement.
constraintParser :: Constants -> Parser Constraint
constraintParser constants =
    keyword "constraint"
        *> constraintBody constants
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
        *> (item `sepBy1` commaTok)
        <* symbol "]"

-- Parses a single-bracket macro input: a literal list [e1, e2, ...]
-- or the name of a whole array, as in allDifferent(q).
macroInput :: Parser a -> Parser (MacroInput a)
macroInput item =
    (Explicit <$> listParser item) <|> (WholeArray <$> identifier)

-- Parses a generator such as (i in 1..n)(q[i] + i). The domain may
-- use declared constants and may be a range or a finite set. The
-- parts do not depend on each other, so applicative style is enough.
generatorParser :: Constants -> Parser a -> Parser (MacroInput a)
generatorParser constants item =
    Generator
        <$> (symbol "(" *> identifier)
        <*> (keyword "in" *> domainParser constants <* symbol ")")
        <*> parenthesised item

-- Parses the input of a macro call in any of its three forms. Every
-- macro goes through here, so all of them support every form.
macroArgs :: Constants -> Parser a -> Parser (MacroInput a)
macroArgs constants item =
    generatorParser constants item <|> parenthesised (macroInput item)

-- Parses a named macro call: name(input).
macroCall :: Constants -> String -> Parser a -> Parser (MacroInput a)
macroCall constants name item = keyword name *> macroArgs constants item

-- Parses the name of a numeric macro.
numericMacroName :: Parser NumericMacro
numericMacroName =
        (Sum <$ keyword "sum")
    <|> (Minimum <$ keyword "min")
    <|> (Maximum <$ keyword "max")

-- Parses a numeric macro as an expression.
numericMacroParser :: Constants -> Parser Expr
numericMacroParser constants =
    NumericMacroExpr
        <$> numericMacroName
        <*> macroArgs constants (expressionParser constants)

-- Parses all, any and allDifferent constraints.
-- Order is irrelevant: keyword checks the whole word.
booleanMacroParser :: Constants -> Parser Constraint
booleanMacroParser constants =
        (All <$> call "all" (comparisonParser constants))
    <|> (Any <$> call "any" (comparisonParser constants))
    <|> (AllDifferent <$> call "allDifferent" (expressionParser constants))
  where
    call = macroCall constants

--------------------------------------------------------------------------------
-- Solve directives
--------------------------------------------------------------------------------

-- Parses one of the three supported solve goals.
solveGoalParser :: Constants -> Parser SolverGoal
solveGoalParser constants =
        (Satisfy <$ keyword "satisfy")
    <|> (Minimize <$> (keyword "minimize" *> expressionParser constants))
    <|> (Maximize <$> (keyword "maximize" *> expressionParser constants))

-- Parses a complete solve directive.
solveDirectiveParser :: Constants -> Parser SolverGoal
solveDirectiveParser constants =
    keyword "solve"
        *> solveGoalParser constants
        <* symbol ";"

--------------------------------------------------------------------------------
-- Scope validation
--------------------------------------------------------------------------------

-- Names declared so far, the values of declared constants, and
-- which names are arrays (so q and q[i] can be told apart).
data Scope = Scope
    { scopeConstants :: Constants
    , scopeNames :: [Identifier]
    , scopeArrays :: [Identifier]
    }

-- The scope at the start of a program.
emptyScope :: Scope
emptyScope = Scope [] [] []

-- How a name is used: as a plain value (x) or as an array (q[i],
-- or q passed whole to a macro).
data Reference
    = ScalarRef Identifier
    | ArrayRef Identifier
    deriving (Eq, Show)

-- References made by an expression.
exprRefs :: Expr -> [Reference]
exprRefs (IntLit _) = []
exprRefs (Var name) = [ScalarRef name]
exprRefs (Add a b) = exprRefs a ++ exprRefs b
exprRefs (Sub a b) = exprRefs a ++ exprRefs b
exprRefs (Mul a b) = exprRefs a ++ exprRefs b
exprRefs (Div a b) = exprRefs a ++ exprRefs b
exprRefs (Pow a b) = exprRefs a ++ exprRefs b
exprRefs (Neg a) = exprRefs a
exprRefs (Mod a b) = exprRefs a ++ exprRefs b
exprRefs (Abs a) = exprRefs a
exprRefs (NumericMacroExpr _ input) = macroRefs exprRefs input
exprRefs (ArrayAccess name index) = ArrayRef name : exprRefs index

-- References made by a macro input. A generator binder is a local
-- scalar, so its uses in the body are removed. It may reuse an outer
-- name: inside the body it shadows that name, matching the evaluator,
-- which puts the binder at the front of the environment. Array
-- elements are stored as q[1], q[2], ..., so a binder named q does
-- not hide the elements of an array q.
macroRefs :: (a -> [Reference]) -> MacroInput a -> [Reference]
macroRefs refs (Explicit items) = concatMap refs items
macroRefs _ (WholeArray name) = [ArrayRef name]
macroRefs refs (Generator name _ item) =
    filter (/= ScalarRef name) (refs item)

-- References made by a constraint.
constraintRefs :: Constraint -> [Reference]
constraintRefs (Compare _ a b) = exprRefs a ++ exprRefs b
constraintRefs (All input) = macroRefs constraintRefs input
constraintRefs (Any input) = macroRefs constraintRefs input
constraintRefs (AllDifferent input) = macroRefs exprRefs input

-- References made by a solve goal.
goalRefs :: SolverGoal -> [Reference]
goalRefs Satisfy = []
goalRefs (Minimize e) = exprRefs e
goalRefs (Maximize e) = exprRefs e

-- Fails unless every reference is declared and used correctly:
-- a scalar use must not name an array, an array use must.
checkReferences :: Scope -> [Reference] -> Parser ()
checkReferences scope refs =
    unless (all validRef refs)
        (failed (UnexpectedString "Invalid identifier use"))
  where
    validRef (ScalarRef name) =
        name `elem` scopeNames scope && name `notElem` scopeArrays scope
    validRef (ArrayRef name) = name `elem` scopeArrays scope

-- Records a new name, rejecting duplicates.
addName :: Scope -> Identifier -> Parser Scope
addName scope name = do
    when (name `elem` scopeNames scope)
        (failed (UnexpectedString ("Duplicate identifier " ++ name)))
    pure scope {scopeNames = name : scopeNames scope}

-- Adds a declaration to the scope. Constants are evaluated using
-- earlier constants only, so references to variables, undeclared names
-- and division by zero are all rejected here.
declare :: Scope -> Declaration -> Parser Scope
declare scope (ConstantDeclaration name e) = do
    value <-
        maybe (failed (UnexpectedString "Invalid constant")) pure
            (evalExpr (scopeConstants scope) e)
    named <- addName scope name
    pure named {scopeConstants = (name, value) : scopeConstants named}
declare scope (VariableDeclaration name _) = addName scope name
declare scope (ArrayDeclaration name _ _) = do
    named <- addName scope name
    pure named {scopeArrays = name : scopeArrays named}

--------------------------------------------------------------------------------
-- Full program
--------------------------------------------------------------------------------

-- Parses any declaration, resolving domains with known constants.
declarationParser :: Scope -> Parser Declaration
declarationParser scope =
    constantDeclaration constants
        <|> variableDeclaration constants
        <|> arrayDeclaration constants
  where
    constants = scopeConstants scope

-- Runs p, then checks every name it references (see Reference).
checked :: (a -> [Reference]) -> Scope -> Parser a -> Parser a
checked refs scope p = do
    x <- p
    checkReferences scope (refs x)
    pure x

-- Parses statements in any order until the final solve directive,
-- which must be followed by end of input. A declaration changes the
-- scope for later statements, so it needs monadic bind; a constraint
-- does not, so applicative style is enough.
program :: Scope -> Parser ADT
program scope =
    (ADT [] [] <$> checked goalRefs scope solveDirective <* eof)
        <|> withDeclaration
        <|> withConstraint
  where
    withDeclaration = do
        d <- declarationParser scope
        next <- declare scope d
        addDeclaration d <$> program next
    withConstraint =
        addConstraint
            <$> checked constraintRefs scope (constraintParser constants)
            <*> program scope
    addDeclaration d adt = adt {declarations = d : declarations adt}
    addConstraint c adt = adt {constraints = c : constraints adt}
    constants = scopeConstants scope
    solveDirective = solveDirectiveParser constants

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- Parses a complete MiniZinc-Lite model.
miniZincParser :: Parser ADT
miniZincParser = spaces *> program emptyScope
