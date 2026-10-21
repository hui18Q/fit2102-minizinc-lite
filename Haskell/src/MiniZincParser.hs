module MiniZincParser
    ( miniZincParser
    ) where

import Instances (ParseError (UnexpectedString), Parser)
import Parser (failed)

import MiniZincADT

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")
