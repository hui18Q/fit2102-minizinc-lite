-- AI usage: ChatGPT and Claude helped organise part of the module
-- re-exports and write formatSolution.
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
    , formatSolution
    ) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import MiniZincADT
import MiniZincParser (miniZincParser)
import MiniZincSolver (solveProblem)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- Contents of a saved solution file: a header with the time it was
-- generated, a blank line, then one "name = value;" line per binding.
-- Pure, so the file format can be tested without doing any IO.
formatSolution :: String -> [(String, Int)] -> String
formatSolution time bindings =
    unlines (("-- Generated: " ++ time) : "" : map binding bindings)
  where
    binding (name, value) = name ++ " = " ++ show value ++ ";"
