module Parser where
import Text.Parsec.String
import Data.Char
import Text.Parsec
import Control.Monad

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
 deriving (Show, Eq, Read)

data Instruction
 = MOV
 | ADD
 deriving (Show, Eq, Read)

data Operation = TwoOp Instruction Operand Operand deriving (Show, Eq, Read)

type Program = [Operation]

-- ************************************************************************** --

-- Eats up all available whitespace at start of stream.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- Parses with p but trims whitespace afterwards.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

-- Parses a label.
label :: Parser String
label = many1 upper

-- Reads a register token
register :: Parser Register
register = do
  n <- char 'R'
  i <- oneOf "1234"
  return $ read [n,i]

-- Parses an operand in an expression in mode0.
mode0 :: Parser Operand
mode0 = Mode0 <$> register

-- Parses an operand as mode1.
mode1 :: Parser Operand
mode1 = do
  void $ lexeme$  char '('
  r <- lexeme register
  void $ lexeme$  char ')'
  return $ Mode1 r

-- Parsers a number.
number :: Parser Integer
number = read <$> many1 digit
-- Parses a single instruction.
instruction :: Parser Instruction
instruction = read <$> choice [string "MOV", string "ADD"]

operand :: Parser Operand
operand = choice [mode0, mode1]

-- Parses a complete two-operand instruction.
operation2 :: Parser Operation
operation2 = TwoOp <$> lexeme instruction <*> lexeme operand <*> lexeme operand

program :: Parser Program
program = many1 operation2
