module MiniZincEval
    ( Env
    , EvalStatus (..)
    , evalPartial
    , evalExpr
    ) where

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
-- Macros (Phase 3.2) and arrays (Part D) come later.
evalPartial _ (NumericMacroExpr _ _) = Unknown
evalPartial _ (ArrayAccess _ _) = Unknown

-- Evaluates a complete expression; Nothing if any part is
-- unassigned or invalid. Used by the parser for constants.
evalExpr :: Env -> Expr -> Maybe Int
evalExpr env expr = case evalPartial env expr of
    Known value -> Just value
    _ -> Nothing
