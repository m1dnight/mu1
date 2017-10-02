module Assembler where

import AST

import Control.Monad.Trans.State.Lazy
import Data.ByteString as BS
import Data.Map as M
import Data.Map.Strict
import Data.Word
import Data.Bits
import Util
import Data.Maybe

import Debug.Trace
import Text.Printf

----------------------------------
-- The state during compilation --
----------------------------------

data CompileState =
  M {
      backpatches :: M.Map Int String, -- Maps line numbers to labels
      linecount   :: Int,              -- Current number of bytes written
      labels      :: M.Map String Int  -- Maps labels to line numbers
    } deriving (Show)

emptyState = M M.empty 0 M.empty

type Memory = CompileState

type Compiler a = State Memory a

type Binary = Word16

type Binaries = [Binary]

binaryStr :: Binary -> String
binaryStr = printf "%016b"

printBinary :: Binary -> String
printBinary b = printf "%05d %s" b (binaryStr b)

printBinaries :: Binaries -> String
printBinaries bs = unlines $ Prelude.map printBinary bs

-------------
-- Helpers --
-------------

-- Manage the current line count.
lineCount :: Compiler Int
lineCount = fmap linecount get

-- Incremenets the current linecount with delta.
lineCountInc :: Int -> Compiler ()
lineCountInc delta =
  do
    state <- get
    let lc = linecount state
    let state' = state {linecount = lc + delta}
    put state'

    -- Remembers a constant
setLabel :: String -> Int -> Compiler ()
setLabel label value =
  do
    state <- get
    let labels' = M.insert label value $ labels state
    let state'   = state { labels = labels' }
    put state'

---------------
-- Interface --
---------------

assemble :: Program -> (Binaries, Memory)
assemble p = let (bs, mem) = runState (assembleProgram p) emptyState
             in
               (bs, mem)

-------------
-- Program --
-------------

assembleProgram :: Program -> Compiler Binaries
assembleProgram []     = return []
assembleProgram (i:is) = do
  bs  <- assembleInstruction i
  bs' <- assembleProgram is
  return $ bs ++ bs'

-----------------
-- Instruction --
-----------------

assembleInstruction :: Instruction -> Compiler Binaries
assembleInstruction (Op o) = assembleOperation o
assembleInstruction (LOp o) = assembleLabeledOperation o

-----------------------
-- Labeled Operation --
-----------------------

assembleLabeledOperation :: LabeledOperation -> Compiler Binaries
assembleLabeledOperation (Labeled l op) = do
  assembleLabel l
  assembleOperation op

---------------
-- Operation --
---------------

assembleOperation :: Operation -> Compiler Binaries
assembleOperation (TwoOp opr src dst) = do
  (srcb, srcr) <- assembleOperand src
  (dstb, dstr) <- assembleOperand dst
  oprb <- flip shift 12 <$> assembleOperator opr
  return $ [shift srcb 6 .|. dstb .|. oprb] ++ maybeToList srcr ++ maybeToList dstr

assembleOperation (OneOp opr src) = do
  srcb <- inlineOperand src
  oprb <- flip shift 12 <$> assembleOperator opr
  return [srcb .|. oprb]

assembleOperation (ZeroOp opr) = do
  opr <- flip shift 12 <$> assembleOperator opr
  return [opr]

-----------
-- Label --
-----------

assembleLabel :: Label -> Compiler ()
assembleLabel l = lineCount >>= setLabel l

---------------
-- Operators --
---------------

assembleOperator :: Operator -> Compiler Binary
assembleOperator MOV  = return 0
assembleOperator ADD  = return 1
assembleOperator SUB  = return 2
assembleOperator CMP  = return 3
assembleOperator BEQ  = return 4
assembleOperator STOP = return 5

--------------
-- Operands --
--------------

assembleOperand :: Operand -> Compiler (Binary, Maybe Binary)
assembleOperand (Mode0 r) = do
  r <- assembleRegister r
  return (r, Nothing)

assembleOperand (Mode1 r) = do
  r <- assembleRegister r
  return (shift 1 3 .|. r, Nothing)

assembleOperand (Mode2 r) = do
  r <- assembleRegister r
  return (shift 2 3 .|. r, Nothing)

assembleOperand (Immed n) = do
  (op, _) <- assembleOperand (Mode2 PC)
  return (op, Just . fromIntegral $ n)

-- If we assemble a label we need to write a dummy value.
-- If we return the value 63, that is the binary pattern 111 111.
-- Addressing mode 111 does not exist, neither does register 111.
-- We assume that this label will be inserted as an offset inline!
assembleOperand (Lbl l) = return (63, Nothing)


inlineOperand :: Operand -> Compiler Binary
inlineOperand (Immed n) = return (fromIntegral n :: Binary)

---------------
-- Registers --
---------------

assembleRegister :: Register -> Compiler Binary
assembleRegister R1 = return 1
assembleRegister R2 = return 2
assembleRegister R3 = return 3
assembleRegister R4 = return 4
assembleRegister PC = return 5
