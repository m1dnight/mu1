module AST where

data Register
 = R1
 | R2
 | R3
 | R4
 | PC
 deriving (Show, Eq, Read)

data Operand
 = Mode0 Register
 | Mode1 Register
 | Immed Int
 deriving (Show, Eq, Read)

data Operator
 = MOV
 | ADD
 deriving (Show, Eq, Read)

type Label = String

data Operation
  = TwoOp Operator Operand Operand
  deriving (Show, Eq, Read)

data LabeledOperation
  = Labeled Label Operation
  deriving (Show, Eq, Read)

data Instruction
  = Op Operation
  | LOp LabeledOperation
  deriving(Show, Eq, Read)


type Program = [Instruction]
