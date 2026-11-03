module MiniZincADT
    ( Identifier
    , ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , MacroInput (..)
    , NumericMacro (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    ) where

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

-- A parsed model: its declarations, constraints and solve goal.
data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

-- Constants keep their expression; domains and array lengths are resolved
-- to numbers while parsing.
data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    | ArrayDeclaration Identifier Int Domain
    deriving (Eq, Show)

-- Represents the allowed values of a variable.
data Domain
    = Range Int Int
    | FiniteSet [Int]
    deriving (Eq, Show)

-- Represents literal, array, and generator macro inputs.
data MacroInput a
    = Explicit [a]
    | WholeArray Identifier
    | Generator Identifier Domain a
    deriving (Eq, Show)

-- The numeric macros sum, min and max.
data NumericMacro = Sum | Minimum | Maximum
    deriving (Eq, Show)

-- Integer expressions. Numeric macros are expressions so they can nest.
data Expr
    = IntLit Int
    | Var Identifier
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Neg Expr
    | Mod Expr Expr -- Part E: x mod y (Haskell mod, sign of the divisor)
    | Abs Expr -- Part E: abs(x)
    | NumericMacroExpr NumericMacro (MacroInput Expr)
    | ArrayAccess Identifier Expr
    deriving (Eq, Show)

-- The six comparison operators (MiniZinc-Lite has no ==).
data CompareOperator
    = OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    deriving (Eq, Show)

-- all and any take conditions; allDifferent takes expressions.
data Constraint = Compare CompareOperator Expr Expr
    | All (MacroInput Constraint)
    | Any (MacroInput Constraint)
    | AllDifferent (MacroInput Expr)
    deriving (Eq, Show)

-- Find any solution, or one that minimises or maximises an expression.
data SolverGoal = Satisfy
    | Minimize Expr
    | Maximize Expr
    deriving (Eq, Show)
