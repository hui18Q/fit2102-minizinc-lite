module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import Instances (ParseError (UnexpectedString), Parser)
import Parser (failed)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    deriving (Eq, Show)

data Domain  --Algebraic Data type for Domain
    = Range Int Int -- Accept a range of integers from low to high ; Range 1 5 = [1,2,3,4,5]
    | FiniteSet [Int] -- Accept a finite set of integers ; FiniteSet [1,3,5] = [1,3,5]
    deriving (Eq, Show)

data Expr
    = IntLit Int
    | Var Identifier
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Neg Expr
    deriving (Eq, Show)

data CompareOperator = OpEq   | OpNe | OpLt | OpLe | OpGt | OpGe
    deriving (Eq, Show)

data Constraint = Compare CompareOperator Expr Expr
    deriving (Eq, Show)

data SolverGoal = Satisfy
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")

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
        v <- domainValues p dom
        search
            p
                { remainingDeclarations = rest
                , solvedVars = (name, v) : solvedVars p
                }

domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues _ (Range lo hi) = [lo..hi]
domainValues _ (FiniteSet xs) = xs --Convert Domain into integer list that solver can access

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

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNe = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
