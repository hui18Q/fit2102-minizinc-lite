-- AI usage: ChatGPT and Claude helped design part of the
-- interval arithmetic for bounds reasoning.

-- | Part E: bounds reasoning. Intervals over-approximate the values an
-- expression can still take, so a branch is pruned only when a comparison
-- cannot hold. If safe bounds cannot be found, nothing is pruned.
module MiniZincBounds
    ( VarBounds
    , domainInterval
    , impossible
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)

import MiniZincADT
import MiniZincEval
    (ArrayLengths, Env, arrayIndices, elementName, firstIndex, macroItems)

-- | An inclusive range [low, high] of possible values. Integer is used
-- so that products of large bounds cannot overflow.
data Interval = Interval Integer Integer
    deriving (Eq, Show)

-- | The domain interval of every decision variable (array elements are
-- listed individually as q[1], q[2], ...).
type VarBounds = [(Identifier, Interval)]

-- | The interval of a single known value.
point :: Int -> Interval
point v = Interval (toInteger v) (toInteger v)

-- | The smallest interval containing both arguments.
hull :: Interval -> Interval -> Interval
hull (Interval a b) (Interval c d) = Interval (min a c) (max b d)

-- | Combines the low ends and the high ends separately. Correct for
-- operations that never decrease when either argument grows, such as
-- (+), min and max.
pointwise :: (Integer -> Integer -> Integer) -> Interval -> Interval -> Interval
pointwise f (Interval a b) (Interval c d) = Interval (f a c) (f b d)

contains :: Interval -> Integer -> Bool
contains (Interval a b) x = a <= x && x <= b

-- | Keeps an interval only if it lies within the range of Int. Outside
-- it the evaluator's Int arithmetic may wrap around, so exact Integer
-- bounds would no longer describe the values it computes.
fitsInt :: Interval -> Maybe Interval
fitsInt i@(Interval a b)
    | toInteger (minBound :: Int) <= a && b <= toInteger (maxBound :: Int) =
        Just i
    | otherwise = Nothing

-- | Applies f to every pair of endpoints and keeps the extremes. Correct
-- for operations monotone in each argument, such as (*), and div when
-- the divisor interval excludes 0.
corners :: (Integer -> Integer -> Integer) -> Interval -> Interval -> Interval
corners f (Interval a b) (Interval c d) =
    Interval (minimum values) (maximum values)
  where
    values = [f x y | x <- [a, b], y <- [c, d]]

-- | The interval of a domain; Nothing for an empty domain.
domainInterval :: Domain -> Maybe Interval
domainInterval (Range lo hi)
    | lo <= hi = Just (Interval (toInteger lo) (toInteger hi))
domainInterval (FiniteSet xs@(_ : _)) =
    Just (Interval (toInteger (minimum xs)) (toInteger (maximum xs)))
domainInterval _ = Nothing

-- | Possible values of an expression, or Nothing when no safe interval is
-- known. Names in env (assigned values, constants, generator binders) are
-- points; other variables use their domain interval.
boundsOf :: ArrayLengths -> VarBounds -> Env -> Expr -> Maybe Interval
boundsOf arrays vars env = go
  where
    -- Every intermediate result must fit in Int, so that no completion
    -- of the assignment can overflow and the evaluator agrees with us.
    go e = fitsInt =<< step e
    step (IntLit n) = Just (point n)
    step (Var name) = (point <$> lookup name env) <|> lookup name vars
    step (Add a b) = pointwise (+) <$> go a <*> go b
    step (Sub a b) = pointwise (+) <$> go a <*> (neg <$> go b)
    step (Mul a b) = corners (*) <$> go a <*> go b
    -- The divisor's interval decides whether division is safe at all, so
    -- the later steps depend on it (monadic bind on Maybe). A divisor
    -- that may be 0 gives no bounds.
    step (Div a b) = do
        divisor <- go b
        guard (not (divisor `contains` 0))
        dividend <- go a
        pure (corners div dividend divisor)
    step (Pow _ _) = Nothing
    step (Neg a) = neg <$> go a
    step (Abs a) = magnitude <$> go a
    step (Mod _ b) = do
        divisor <- go b
        guard (not (divisor `contains` 0))
        pure (remainderRange divisor)
    step (NumericMacroExpr macro input) =
        aggregate macro =<< nonEmpty =<< sequenceA (itemBounds input)
    step (ArrayAccess name index) = do
        len <- lookup name arrays
        Interval lo hi <- go index
        -- Indices outside the array make the assignment invalid anyway,
        -- so only in-bounds elements can contribute a value.
        let indices = [max (toInteger firstIndex) lo .. min (toInteger len) hi]
        elements <- traverse (go . Var . elementName name . fromInteger) indices
        foldr1 hull <$> nonEmpty elements

    -- Bounds of each item of a numeric macro input.
    itemBounds (WholeArray name) = case lookup name arrays of
        Just len -> [go (ArrayAccess name (IntLit i)) | i <- arrayIndices len]
        Nothing -> [Nothing]
    itemBounds input =
        [ boundsOf arrays vars itemEnv item
        | (itemEnv, item) <- macroItems env input
        ]

neg :: Interval -> Interval
neg (Interval a b) = Interval (negate b) (negate a)

-- | Interval of the absolute value of every value.
magnitude :: Interval -> Interval
magnitude i@(Interval a b)
    | a >= 0 = i
    | b <= 0 = neg i
    | otherwise = Interval 0 (max (negate a) b)

-- | Possible results of x `mod` d for d in a divisor interval that
-- excludes 0. Haskell's mod takes the sign of the divisor and is smaller
-- than it in size, so the result depends only on the divisor.
remainderRange :: Interval -> Interval
remainderRange (Interval c d)
    | c > 0 = Interval 0 (d - 1)
    | otherwise = Interval (c + 1) 0

-- | Interval of sum, min or max. Every partial sum, in any order, lies
-- between the sum of negative lows and the sum of positive highs, so
-- checking that range rules out overflow inside the sum.
aggregate :: NumericMacro -> NonEmpty Interval -> Maybe Interval
aggregate Sum items = foldr1 (pointwise (+)) items <$ fitsInt partialSums
  where
    partialSums =
        Interval
            (sum [min a 0 | Interval a _ <- toList items])
            (sum [max b 0 | Interval _ b <- toList items])
aggregate Minimum items = Just (foldr1 (pointwise min) items)
aggregate Maximum items = Just (foldr1 (pointwise max) items)

-- | True when a comparison is false for every pair of values drawn from
-- the two intervals.
cannotHold :: CompareOperator -> Interval -> Interval -> Bool
cannotHold OpEq (Interval a b) (Interval c d) = b < c || d < a
cannotHold OpNe (Interval a b) (Interval c d) = a == b && c == d && a == c
cannotHold OpLt (Interval a _) (Interval _ d) = a >= d
cannotHold OpLe (Interval a _) (Interval _ d) = a > d
cannotHold OpGt (Interval _ b) (Interval c _) = b <= c
cannotHold OpGe (Interval _ b) (Interval c _) = b < c

-- | True when the bounds prove that a constraint can no longer hold.
-- all fails if any element is impossible; any fails only if every
-- element is. allDifferent is left to the evaluator's own check.
impossible :: ArrayLengths -> VarBounds -> Env -> Constraint -> Bool
impossible arrays vars env constraint = case constraint of
    Compare op a b ->
        fromMaybe False (cannotHold op <$> bounds env a <*> bounds env b)
    All input -> any impossibleItem (macroItems env input)
    Any input ->
        let items = macroItems env input
         in not (null items) && all impossibleItem items
    AllDifferent _ -> False
  where
    bounds = boundsOf arrays vars
    impossibleItem (itemEnv, item) = impossible arrays vars itemEnv item
