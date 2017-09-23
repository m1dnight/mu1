module Assembler where

import AST

import Control.Monad.Trans.State.Lazy
import Data.ByteString as BS
import Data.Map as M
import Data.Map.Strict
import Data.Word
import Data.Bits
import Util

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

test :: Operation -> Binary
test o = evalState (assembleOperation o) emptyState

test1 = test $ TwoOp MOV (Mode0 R1) (Mode0 R2)

printBinary :: Word16 -> String
printBinary = printf "%016b"

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

-------------
-- Program --
-------------
assembleOperation :: Operation -> Compiler Binary
assembleOperation (TwoOp opr src dst) = do
  srcb <- flip shift 6 <$> assembleOperand src
  dstb <- assembleOperand dst
  oprb <- flip shift 12 <$> assembleOperator opr
  trace (printBinary srcb ++ " " ++ printBinary dstb ++ " " ++ printBinary oprb) $ return $ srcb .|. dstb .|. oprb

---------------
-- Operators --
---------------

assembleOperator :: Operator -> Compiler Binary
assembleOperator MOV = return 0
assembleOperator ADD = return 1
assembleOperator SUB = return 2
assembleOperator CMP = return 3
assembleOperator BEQ = return 4

--------------
-- Operands --
--------------

assembleOperand :: Operand -> Compiler Binary
assembleOperand (Mode0 r) = do
  r <- assembleRegister r
  return r

assembleOperand (Mode1 r) = do
  r <- assembleRegister r
  return $ shift 3 1 .|. r

---------------
-- Registers --
---------------

assembleRegister :: Register -> Compiler Binary
assembleRegister R1 = return 1
assembleRegister R2 = return 2
assembleRegister R3 = return 3
assembleRegister R4 = return 4
assembleRegister PC = return 5
