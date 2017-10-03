module Parser where
import Text.Parsec.String
import Data.Char
import Text.Parsec hiding (label)
import Control.Monad
import AST

-- ************************************************************************** --

-- Eats up all available whitespace at start of stream.
whitespace :: Parser ()
whitespace = void $ many (inlineComment
                          <|> simpleWhitespace
                          <|> blockComment
                          <|> unexpected "foo")

simpleWhitespace :: Parser ()
simpleWhitespace = void $ many1 $ oneOf " \n\t"

inlineComment :: Parser ()
inlineComment = void (many1 $ char ';' <* manyTill anyChar (void (char '\n') <|> eof))


blockComment :: Parser ()
blockComment = void (-- no nesting of block comments in SQL
                     try (string "/*")
                     *> manyTill anyChar (try $ string "*/"))


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
number = negative <|> positive

positive :: Parser Operand
positive = Immed . read <$> many1 digit

negative :: Parser Operand
negative  = void (char '-') >> Immed . read <$> many1 digit

-- Parses a label.
label :: Parser Label
label = do
  l <- lexeme $ many1 upper
  void $ lexeme $ char ':'
  return l

labelOperand :: Parser Operand
labelOperand = do
  l <- lexeme $ many1 upper
  return $ Lbl l

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
operator2 :: Parser Operator
operator2 = read <$> choice [try $ string "MOV", try $ string "ADD",
                            try $ string "SUB", try $ string "CMP"]

operator1 :: Parser Operator
operator1 = read <$> choice [try $ string "BEQ", try $ string "BNE"]

operator0 :: Parser Operator
operator0 = read <$> choice [try $ string "STOP"]

------------------
-- Parser Rules --
------------------

-- Parses an operand. Either a register or an immediate value.
operand :: Parser Operand
operand = choice [try mode0, try mode1, try number, try labelOperand]

-- Parsers a complete zero-operand operation.
operation0 :: Parser Operation
operation0 = ZeroOp <$> lexeme operator0

-- Parsers a complete one-operand operation.
operation1 :: Parser Operation
operation1 = OneOp <$> lexeme operator1 <*> lexeme operand

-- Parses a complete two-operand operation.
operation2 :: Parser Operation
operation2 = TwoOp <$> lexeme operator2 <*> lexeme operand <*> lexeme operand

-- Parses any operation (0, 1, or two operands)
operation :: Parser Operation
operation = choice [try operation2, try operation1, try operation0]

unlabeledOperation :: Parser Instruction
unlabeledOperation = Op <$> operation

labeledOperation :: Parser Instruction
labeledOperation = LOp <$> (Labeled <$> label <*> operation)

-- Parses any operation.
-- Labeled operations, or none-labeled.
instruction :: Parser Instruction
instruction = try unlabeledOperation <|> labeledOperation

-- Entry point of the parser.
program :: Parser Program
program = whitespace >> many1 instruction <* eof
