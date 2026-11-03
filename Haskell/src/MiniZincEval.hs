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
    , macroItems
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

-- Invalid on either side wins over Unknown, even if the other is unknown.
-- No Monad instance on purpose: Unknown >>= f cannot look past Unknown
-- to find an Invalid, so ap would disagree with <*>.
instance Applicative EvalStatus where
    pure :: a -> EvalStatus a
    pure = Known

    (<*>) :: EvalStatus (a -> b) -> EvalStatus a -> EvalStatus b
    Invalid <*> _ = Invalid
    _ <*> Invalid = Invalid
    Known f <*> Known x = Known (f x)
    _ <*> _ = Unknown

-- Evaluates an expression under a partial assignment.
-- go captures arrays and env, so the recursive cases only pass sub-expressions.
evalPartial :: ArrayLengths -> Env -> Expr -> EvalStatus Int
evalPartial arrays env = go
  where
    go (IntLit n) = Known n
    go (Var name) = maybe Unknown Known (lookup name env)
    go (Add a b) = (+) <$> go a <*> go b
    go (Sub a b) = (-) <$> go a <*> go b
    go (Mul a b) = (*) <$> go a <*> go b
    go (Div a b) = divideBy div a b
    go (Mod a b) = divideBy mod a b
    go (Pow a b) = case go b of
        Known e | e < 0 -> Invalid
        power -> (^) <$> go a <*> power
    go (Neg a) = negate <$> go a
    go (Abs a) = abs <$> go a
    go (NumericMacroExpr macro input) =
        numericMacro macro (exprItems arrays env input)
    -- The index must be known before we know which element to read,
    -- so this step is sequential and cannot use <*>.
    go (ArrayAccess name index) = case go index of
        Known i -> element arrays env name i
        Unknown -> Unknown
        Invalid -> Invalid

    -- div and mod: a zero divisor is invalid even if the numerator is unknown.
    -- The result is computed as an Integer because minBound `div` (-1)
    -- overflows Int and would throw; a result outside Int is invalid.
    divideBy op a b = case (go a, go b) of
        (_, Known 0) -> Invalid
        (Known x, Known y) ->
            fromIntegerChecked (op (toInteger x) (toInteger y))
        -- Otherwise one side is Unknown or Invalid; *> keeps Invalid.
        (x, y) -> x *> y

-- An Integer result as an Int, or Invalid if it does not fit.
fromIntegerChecked :: Integer -> EvalStatus Int
fromIntegerChecked r
    | toInteger (minBound :: Int) <= r && r <= toInteger (maxBound :: Int) =
        Known (fromInteger r)
    | otherwise = Invalid

-- Reads element i of an array: Invalid if the index is out of
-- bounds (even before the element is assigned), Unknown if the
-- element is not assigned yet.
element :: ArrayLengths -> Env -> Identifier -> Int -> EvalStatus Int
element arrays env name i = case lookup name arrays of
    Just len | 1 <= i && i <= len ->
        maybe Unknown Known (lookup (elementName name i) env)
    _ -> Invalid

-- Evaluates a complete expression; Nothing if any part is unassigned
-- or invalid. Used for constants, which cannot refer to arrays, so no
-- array lengths are needed.
evalExpr :: Env -> Expr -> Maybe Int
evalExpr env expr = case evalPartial [] env expr of
    Known value -> Just value
    _ -> Nothing

--------------------------------------------------------------------------------
-- Macros and constraints
--------------------------------------------------------------------------------

-- Items of a list or generator input, each with the environment it is
-- evaluated in. A generator binder is put at the front of that
-- environment, so it shadows outer names. Shared with bounds reasoning.
macroItems :: Env -> MacroInput a -> [(Env, a)]
macroItems env (Explicit items) = map ((,) env) items
macroItems env (Generator name dom item) =
    [((name, v) : env, item) | v <- domainValues dom]
macroItems _ (WholeArray _) = []

-- Evaluates every item of a macro input.
macroResults
    :: (Env -> a -> EvalStatus b) -> Env -> MacroInput a -> [EvalStatus b]
-- A whole array of numbers is handled by exprItems; reaching here
-- means a whole array where conditions are expected, e.g. all(q).
macroResults _ _ (WholeArray _) = [Invalid]
macroResults eval env input =
    [eval itemEnv item | (itemEnv, item) <- macroItems env input]

-- Results of the items of a numeric macro input. A whole array is
-- read element by element; lists and generators use macroResults.
exprItems :: ArrayLengths -> Env -> MacroInput Expr -> [EvalStatus Int]
exprItems arrays env (WholeArray name) = arrayElements arrays env name
exprItems arrays env input = macroResults (evalPartial arrays) env input

-- Every element of an array in index order, so unassigned elements
-- stay Unknown and partial pruning still works.
-- Invalid if there is no such array (or its length is not positive).
arrayElements :: ArrayLengths -> Env -> Identifier -> [EvalStatus Int]
arrayElements arrays env name = case lookup name arrays of
    Just len | len >= 1 -> [element arrays env name i | i <- [1 .. len]]
    _ -> [Invalid]

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

-- Evaluates a constraint under a partial assignment. Known False and
-- Invalid are final, but Known True is not: an unknown part may still
-- become Invalid, so the solver re-checks constraints at full assignments.
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
    go (AllDifferent input) = allDifferentStatus (exprItems arrays env input)

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
