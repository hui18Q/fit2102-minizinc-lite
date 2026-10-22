module MiniZincSolver
    ( solveProblem
    ) where

import Data.Maybe (listToMaybe)

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

-- Explores assignments depth-first with the list monad. Every call
-- first checks the constraints, so a branch is abandoned as soon as
-- the latest assignment makes some constraint fail.
search :: ConstraintProblem -> [[Binding]]
search p
    | not (consistent p) = []
    | otherwise = case remainingDeclarations p of
        [] -> [reverse (solvedVars p) | allSatisfied p]
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
