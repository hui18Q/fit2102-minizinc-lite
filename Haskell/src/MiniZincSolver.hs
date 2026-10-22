module MiniZincSolver
    ( solveProblem
    ) where

import Data.List (minimumBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..), comparing)

import MiniZincADT
import MiniZincEval
    ( Env
    , EvalStatus (..)
    , domainValues
    , evalConstraintPartial
    , evalExpr
    )

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

-- Finds a satisfying or optimal assignment for the model.
-- Satisfy takes the first solution, so laziness stops the search
-- early; optimisation has to compare every solution.
solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt = case solverGoal adt of
    Satisfy -> solution <$> listToMaybe results
    Minimize objective -> optimal id objective results
    Maximize objective -> optimal Down objective results
  where
    results = search problem
    problem =
        ConstraintProblem
            { searchConstants = []
            , solvedVars = []
            , remainingDeclarations = declarations adt
            , searchConstraints = constraints adt
            }

-- Explores assignments depth-first with the list monad. Every call
-- first checks the constraints, so a branch is abandoned as soon as
-- the latest assignment makes some constraint fail.
search :: ConstraintProblem -> [ConstraintProblem]
search p
    | not (consistent p) = []
    | otherwise = case remainingDeclarations p of
        [] -> [p | allSatisfied p]
        ConstantDeclaration name e : rest ->
            case evalExpr (environment p) e of
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
        -- Arrays are implemented in Part D.
        ArrayDeclaration _ _ _ : _ -> []

-- Combines constants and assigned variables into an environment.
environment :: ConstraintProblem -> Env
environment p = solvedVars p ++ searchConstants p

-- Decision variables in declaration order (constants are excluded).
solution :: ConstraintProblem -> [Binding]
solution = reverse . solvedVars

-- Picks the solution whose objective value is smallest under key:
-- id for minimize, Down for maximize (Down reverses the order).
-- Assignments whose objective is invalid are skipped, like any
-- other invalid assignment. nonEmpty means minimumBy never sees an
-- empty list. Ties keep the first solution found.
optimal
    :: Ord k => (Int -> k) -> Expr -> [ConstraintProblem] -> Maybe [Binding]
optimal key objective results =
    solution . snd . minimumBy (comparing (key . fst)) <$> nonEmpty scored
  where
    scored =
        [ (value, p)
        | p <- results
        , Just value <- [evalExpr (environment p) objective]
        ]

-- Evaluates all constraints under the current assignment.
constraintResults :: ConstraintProblem -> [EvalStatus Bool]
constraintResults p =
    map (evalConstraintPartial (environment p)) (searchConstraints p)

-- Reject known failures and invalid evaluations.
-- Unknown and Known True allow the search to continue.
consistent :: ConstraintProblem -> Bool
consistent = all (`notElem` [Known False, Invalid]) . constraintResults

-- Accept only when every constraint is fully satisfied. Checked again
-- once all variables are assigned, because Known True is not final.
allSatisfied :: ConstraintProblem -> Bool
allSatisfied = all (== Known True) . constraintResults
