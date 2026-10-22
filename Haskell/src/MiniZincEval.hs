module MiniZincEval
    ( Env
    , ArrayLengths
    , EvalStatus (..)
    , evalPartial
    , evalExpr
    , evalConstraintPartial
    , domainValues
    , elementName
    , hasDuplicates
    ) where

import Data.List (nub)

import MiniZincADT

-- Name-to-value bindings used during evaluation.
type Env = [(Identifier, Int)]

-- Declared length of each array, used to check index bounds.
type ArrayLengths = [(Identifier, Int)]

-- Evaluation result under a partial assignment.
data EvalStatus a
    = Known a -- fully evaluated
    | Unknown -- depends on an unassigned variable
    | Invalid -- e.g. division by zero; no assignment can fix it
    deriving (Eq, Show)

instance Functor EvalStatus where
    fmap :: (a -> b) -> EvalStatus a -> EvalStatus b
    fmap f (Known x) = Known (f x)
    fmap _ Unknown = Unknown
    fmap _ Invalid = Invalid

-- Invalid on either side wins over Unknown, so an invalid operand
-- is detected even while the other operand is still unknown.
-- There is deliberately no Monad instance: Unknown >>= f cannot
-- look past Unknown to find an Invalid, so ap would disagree
-- with <*>.
instance Applicative EvalStatus where
    pure :: a -> EvalStatus a
    pure = Known

    (<*>) :: EvalStatus (a -> b) -> EvalStatus a -> EvalStatus b
    Invalid <*> _ = Invalid
    _ <*> Invalid = Invalid
    Known f <*> Known x = Known (f x)
    _ <*> _ = Unknown

-- Evaluates an expression under a partial assignment. go captures
-- arrays and env, so the recursive cases only pass sub-expressions.
evalPartial :: ArrayLengths -> Env -> Expr -> EvalStatus Int
evalPartial arrays env = go
  where
    go (IntLit n) = Known n
    go (Var name) = maybe Unknown Known (lookup name env)
    go (Add a b) = (+) <$> go a <*> go b
    go (Sub a b) = (-) <$> go a <*> go b
    go (Mul a b) = (*) <$> go a <*> go b
    go (Div a b) = case go b of
        Known 0 -> Invalid -- invalid even if the numerator is unknown
        divisor -> div <$> go a <*> divisor
    go (Pow a b) = case go b of
        Known e | e < 0 -> Invalid
        power -> (^) <$> go a <*> power
    go (Neg a) = negate <$> go a
    go (NumericMacroExpr macro input) =
        numericMacro macro (macroResults (evalPartial arrays) env input)
    -- The index must be known before we know which element to read,
    -- so this step is sequential and cannot use <*>.
    go (ArrayAccess name index) = case go index of
        Known i -> element arrays env name i
        Unknown -> Unknown
        Invalid -> Invalid

-- Reads element i of an array: Invalid if the index is out of
-- bounds (even before the element is assigned), Unknown if the
-- element is not assigned yet.
element :: ArrayLengths -> Env -> Identifier -> Int -> EvalStatus Int
element arrays env name i = case lookup name arrays of
    Just len | 1 <= i && i <= len ->
        maybe Unknown Known (lookup (elementName name i) env)
    _ -> Invalid

-- Evaluates a complete expression; Nothing if any part is
-- unassigned or invalid. Used for constants, which cannot refer to
-- arrays, so no array lengths are needed.
evalExpr :: Env -> Expr -> Maybe Int
evalExpr env expr = case evalPartial [] env expr of
    Known value -> Just value
    _ -> Nothing

--------------------------------------------------------------------------------
-- Macros and constraints
--------------------------------------------------------------------------------

-- Evaluates every item of a macro input. A generator binds its
-- loop variable at the front of the environment, so it shadows
-- any outer name with the same identifier.
-- Part D: WholeArray must expand to q[1]..q[n].
macroResults
    :: (Env -> a -> EvalStatus b) -> Env -> MacroInput a -> [EvalStatus b]
macroResults eval env (Explicit items) = map (eval env) items
macroResults eval env (Generator name dom item) =
    [eval ((name, v) : env) item | v <- domainValues dom]
macroResults _ _ (WholeArray _) = [Unknown]

-- The values a domain allows, in increasing order for ranges.
domainValues :: Domain -> [Int]
domainValues (Range lo hi) = [lo .. hi]
domainValues (FiniteSet xs) = xs

-- Aggregates sum, min or max. sequenceA turns the list of results
-- into one result: Invalid if any item is invalid, otherwise
-- Unknown if any item is unknown. An empty input is invalid.
numericMacro :: NumericMacro -> [EvalStatus Int] -> EvalStatus Int
numericMacro macro results
    | null results = Invalid
    | otherwise = aggregate macro <$> sequenceA results
  where
    aggregate Sum = sum
    aggregate Minimum = minimum
    aggregate Maximum = maximum

-- Applies a comparison operator to two integers.
applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNe = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

-- Evaluates a constraint under a partial assignment.
-- Known True is not final: an unknown part may still become
-- Invalid, so the solver must re-check every constraint once
-- all variables are assigned. Known False and Invalid are final.
evalConstraintPartial
    :: ArrayLengths -> Env -> Constraint -> EvalStatus Bool
evalConstraintPartial arrays env = go
  where
    expr = evalPartial arrays env
    go (Compare op a b) = applyCompare op <$> expr a <*> expr b
    go (All input) =
        allStatus (macroResults (evalConstraintPartial arrays) env input)
    go (Any input) =
        anyStatus (macroResults (evalConstraintPartial arrays) env input)
    go (AllDifferent input) =
        allDifferentStatus (macroResults (evalPartial arrays) env input)

-- True for an invalid result.
isInvalid :: EvalStatus a -> Bool
isInvalid Invalid = True
isInvalid _ = False

-- all: one known False decides it, even with unknowns left.
-- Invalid anywhere makes it invalid; an empty input is invalid.
allStatus :: [EvalStatus Bool] -> EvalStatus Bool
allStatus results
    | null results = Invalid
    | any isInvalid results = Invalid
    | Known False `elem` results = Known False
    | all (== Known True) results = Known True
    | otherwise = Unknown

-- any xs = not (all (map not xs)), by De Morgan's law.
anyStatus :: [EvalStatus Bool] -> EvalStatus Bool
anyStatus = fmap not . allStatus . map (fmap not)

-- allDifferent: two equal known values already violate it,
-- even while other elements are still unassigned.
allDifferentStatus :: [EvalStatus Int] -> EvalStatus Bool
allDifferentStatus results
    | null results = Invalid
    | any isInvalid results = Invalid
    | hasDuplicates known = Known False
    | length known == length results = Known True
    | otherwise = Unknown
  where
    known = [v | Known v <- results]

-- True when a list contains a repeated element.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- The variable name of an array element, e.g. elementName "q" 1 = "q[1]".
-- The solver uses it to name expanded elements and the evaluator to
-- look them up, so both always agree. It is also the output format.
elementName :: Identifier -> Int -> Identifier
elementName name i = name ++ "[" ++ show i ++ "]"
