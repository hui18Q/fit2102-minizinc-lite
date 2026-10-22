module MiniZincEval
    ( Env
    , EvalStatus (..)
    , evalPartial
    , evalExpr
    , evalConstraintPartial
    , domainValues
    , hasDuplicates
    ) where

import Data.List (nub)

import MiniZincADT

-- Name-to-value bindings used during evaluation.
type Env = [(Identifier, Int)]

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

-- Evaluates an expression under a partial assignment.
evalPartial :: Env -> Expr -> EvalStatus Int
evalPartial _ (IntLit n) = Known n
evalPartial env (Var name) = maybe Unknown Known (lookup name env)
evalPartial env (Add a b) = (+) <$> evalPartial env a <*> evalPartial env b
evalPartial env (Sub a b) = (-) <$> evalPartial env a <*> evalPartial env b
evalPartial env (Mul a b) = (*) <$> evalPartial env a <*> evalPartial env b
evalPartial env (Div a b) = case evalPartial env b of
    Known 0 -> Invalid -- invalid even if the numerator is unknown
    divisor -> div <$> evalPartial env a <*> divisor
evalPartial env (Pow a b) = case evalPartial env b of
    Known e | e < 0 -> Invalid
    power -> (^) <$> evalPartial env a <*> power
evalPartial env (Neg a) = negate <$> evalPartial env a
evalPartial env (NumericMacroExpr macro input) =
    numericMacro macro (macroResults evalPartial env input)
-- Array access is implemented in Part D.
evalPartial _ (ArrayAccess _ _) = Unknown

-- Evaluates a complete expression; Nothing if any part is
-- unassigned or invalid. Used by the parser for constants.
evalExpr :: Env -> Expr -> Maybe Int
evalExpr env expr = case evalPartial env expr of
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
evalConstraintPartial :: Env -> Constraint -> EvalStatus Bool
evalConstraintPartial env (Compare op a b) =
    applyCompare op <$> evalPartial env a <*> evalPartial env b
evalConstraintPartial env (All input) =
    allStatus (macroResults evalConstraintPartial env input)
evalConstraintPartial env (Any input) =
    anyStatus (macroResults evalConstraintPartial env input)
evalConstraintPartial env (AllDifferent input) =
    allDifferentStatus (macroResults evalPartial env input)

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
