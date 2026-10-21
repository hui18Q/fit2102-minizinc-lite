module MiniZincSolver
    ( solveProblem
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)

import MiniZincADT

--------------------------------------------------------------------------------
-- Solver
--------------------------------------------------------------------------------

type Binding = (Identifier, Int)

data ConstraintProblem = ConstraintProblem
    { searchConstants :: [Binding]
    , solvedVars :: [Binding]
    , remainingDeclarations :: [Declaration]
    , searchConstraints :: [Constraint]
    }

solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt = listToMaybe (search problem)
  where
    problem =
        ConstraintProblem
            { searchConstants = []
            , solvedVars = []
            , remainingDeclarations = declarations adt
            , searchConstraints = constraints adt
            }

search :: ConstraintProblem -> [[Binding]]
search p = case remainingDeclarations p of
    [] -> if allSatisfied p then [reverse (solvedVars p)] else []
    ConstantDeclaration name e : rest ->
        case evalExpr p e of
            Nothing -> []
            Just v ->
                search
                    p
                        { remainingDeclarations = rest
                        , searchConstants = (name, v) : searchConstants p
                        }
    VariableDeclaration name dom : rest -> do
        v <- domainValues dom
        search
            p
                { remainingDeclarations = rest
                , solvedVars = (name, v) : solvedVars p
                }

    ArrayDeclaration _ _ _ : _ -> []

-- Returns the values allowed by a variable's domain.
domainValues :: Domain -> [Int]
domainValues (Range lo hi) = [lo..hi]
domainValues (FiniteSet xs) = xs

evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr _ (IntLit n) = Just n
evalExpr p (Var x) = lookup x (searchConstants p) <|> lookup x (solvedVars p)
evalExpr p (Add a b) = (+) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Sub a b) = (-) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Mul a b) = (*) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Div a b) = do
    x <- evalExpr p a
    y <- evalExpr p b
    if y == 0 then Nothing else Just (x `div` y)
evalExpr p (Pow a b) = do
    x <- evalExpr p a
    y <- evalExpr p b
    if y < 0 then Nothing else Just (x ^ y)
evalExpr p (Neg a) = negate <$> evalExpr p a
evalExpr _ (NumericMacroExpr _ _) = Nothing
evalExpr _ (ArrayAccess _ _) = Nothing

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) =
    applyCompare op <$> evalExpr p a <*> evalExpr p b
evalConstraint _ (All _) = Nothing
evalConstraint _ (Any _) = Nothing
evalConstraint _ (AllDifferent _) = Nothing

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNe = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)
