module MiniZincEval
    ( Env, evalExpr
    ) where

import MiniZincADT

type Env = [(Identifier, Int)]

-- Evaluates an expression using the given environment, returns Nothing if evaluation is not possible.
evalExpr :: Env -> Expr -> Maybe Int
evalExpr _ (IntLit n) = Just n

evalExpr env (Var x) = lookup x env

evalExpr env (Add a b) =
    (+) <$> evalExpr env a <*> evalExpr env b

evalExpr env (Sub a b) =
    (-) <$> evalExpr env a <*> evalExpr env b

evalExpr env (Mul a b) =
    (*) <$> evalExpr env a <*> evalExpr env b

evalExpr env (Div a b) = do
    x <- evalExpr env a
    y <- evalExpr env b
    if y == 0
        then Nothing
        else Just (x `div` y)

evalExpr env (Pow a b) = do
    x <- evalExpr env a
    y <- evalExpr env b
    if y < 0
        then Nothing
        else Just (x ^ y)

evalExpr env (Neg a) =
    negate <$> evalExpr env a

-- Numeric macros and arrays will be implemented later.
evalExpr _ (NumericMacroExpr _ _) = Nothing
evalExpr _ (ArrayAccess _ _) = Nothing