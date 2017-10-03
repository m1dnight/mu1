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
 | Mode2 Register
 | Immed Int
 | Lbl   Label
 deriving (Show, Eq, Read)

data Operator
 = MOV
 | ADD
 | SUB
 | CMP
 | BEQ
 | STOP
 | BNE
 deriving (Show, Eq, Read)

type Label = String

data Operation
  = TwoOp Operator Operand Operand
  | OneOp Operator Operand
  | ZeroOp Operator
  deriving (Show, Eq, Read)

data LabeledOperation
  = Labeled Label Operation
  deriving (Show, Eq, Read)

data Instruction
  = Op Operation
  | LOp LabeledOperation
  deriving(Show, Eq, Read)

type Program = [Instruction]
