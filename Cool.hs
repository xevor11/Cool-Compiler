module Cool where

type Symbol = String
type Boolean = Bool

data Program = Program [Class]
    deriving (Eq, Show)

data Class = ClassDecl Symbol (Maybe Symbol) [Feature] Symbol
    deriving (Eq, Show)

data Feature = Method Boolean Symbol [Formal] Symbol Expression
             | Attribute Symbol Symbol
    deriving (Eq, Show)

data Formal = Formal Symbol Symbol
    deriving (Eq, Show)

data Expression = Assign Symbol Expression
                | StaticDispatch Expression Symbol Symbol [Expression]
                | Dispatch Expression Symbol [Expression]
                | Cond Expression Expression Expression
                | Loop Expression Expression
                | TypeCase Expression [Case]
                | Block [Expression]
                | Let Symbol Symbol Expression Expression
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Neg Expression
                | Lt Expression Expression
                | Leq Expression Expression
                | Comp Expression
                | IntLit Int
                | BoolLit Bool
                | StringLit String
                | Alloc Symbol
                | Nil
                | Unit
                | NoExpr
                | Variable Symbol
                deriving (Eq, Show)

data Case = Branch Symbol Symbol Expression
    deriving (Eq, Show)
