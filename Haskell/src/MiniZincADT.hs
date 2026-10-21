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

data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

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

data NumericMacro = Sum | Minimum | Maximum
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
    | NumericMacroExpr NumericMacro (MacroInput Expr)
    | ArrayAccess Identifier Expr
    deriving (Eq, Show)

data CompareOperator
    = OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    deriving (Eq, Show)

data Constraint = Compare CompareOperator Expr Expr
    | All (MacroInput Constraint)
    | Any (MacroInput Constraint)
    | AllDifferent (MacroInput Expr)
    deriving (Eq, Show)

data SolverGoal = Satisfy
    | Minimize Expr
    | Maximize Expr
    deriving (Eq, Show)
