module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , MacroInput (..)
    , NumericMacro (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import MiniZincADT
import MiniZincParser (miniZincParser)
import MiniZincSolver (solveProblem)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
