module Parser where
import Text.Parsec.String
import Data.Char
import Text.Parsec hiding (label)
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

-- ************************************************************************** --

-- Eats up all available whitespace at start of stream.
whitespace :: Parser ()
whitespace = try inlineComment <|> try simpleWhitespace <|> return ()

simpleWhitespace :: Parser ()
simpleWhitespace = void $ many $ oneOf " \n\t"

inlineComment :: Parser ()
inlineComment = void (many1 $ char ';' <* manyTill anyChar (void (char '\n') <|> eof))

blockComment :: Parser ()
blockComment = do
  void $ try (string "/*")
  manyTill anyChar (try $ string "*/")
  return ()

-- Parses with p but trims whitespace afterwards.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

-----------
-- Atoms --
-----------

-- Reads a register token
register :: Parser Register
register = do
  n <- char 'R'
  i <- oneOf "1234"
  return $ read [n,i]

-- Parsers a number.
number :: Parser Operand
number = Immed . read <$> many1 digit

------------
-- Tokens --
------------

-- Parses an operand in an expression in mode0.
mode0 :: Parser Operand
mode0 = Mode0 <$> lexeme register

-- Parses an operand as mode1.
mode1 :: Parser Operand
mode1 = do
  void $ lexeme$  char '('
  r <- lexeme register
  void $ lexeme $  char ')'
  return $ Mode1 r

-- Parses a single instruction.
operator :: Parser Operator
operator = read <$> choice [string "MOV", string "ADD"]

------------------
-- Parser Rules --
------------------

-- Parses an operand. Either a register or an immediate value.
operand :: Parser Operand
operand = choice [mode0, mode1, number]



-- Parses a complete two-operand operation.
operation2 :: Parser Operation
operation2 = TwoOp <$> lexeme operator <*> lexeme operand <*> lexeme operand

-- Parses any operation (0, 1, or two operands)
operation :: Parser Operation
operation = choice [operation2]

-- Parses a label.
label :: Parser Label
label = do
  l <- lexeme $ many1 upper
  void $ lexeme $ char ':'
  return l

unlabeledOperation :: Parser Instruction
unlabeledOperation = Op <$> operation

labeledOperation :: Parser Instruction
labeledOperation = LOp <$> (Labeled <$> label <*> operation)

-- Parses any operation.
-- Labeled operations, or none-labeled.
instruction :: Parser Instruction
instruction = unlabeledOperation <|> labeledOperation

-- Entry point of the parser.
program :: Parser Program
program = many1 instruction <* eof
