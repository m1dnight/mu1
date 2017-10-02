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


-- Remembers a label and a current address in the
setBackpatch :: String -> Int -> Compiler ()
setBackpatch label position =
  do
    state <- get
    let patches' = M.insert position label $ backpatches state
    let state'   = state { backpatches = patches' }
    put state'


-- Remembers a constant
setLabel :: String -> Int -> Compiler ()
setLabel label value =
  do
    state <- get
    let labels' = M.insert label value $ labels state
    let state'   = state { labels = labels' }
    put state'


-- Backpatches the entire binary with the state accumulated during compilation.
backpatch :: Binaries -> Compiler Binaries
backpatch src =
  do
    state <- get
    -- ls :: Map String Int
    let ls  = labels state
    -- bps :: Map Int String
    let bps = backpatches state
    -- Iterate over all the labels and replace them if they have been referenced.
    let patched = M.foldWithKey (\currentLine label bin ->
                                   -- We need to backpatch on line "currentLine".
                                   -- The line the label refers to is 'lblline'.
                                   let (Just target) = M.lookup label ls
                                       offset        = compress2c (fromIntegral (target - currentLine))
                                   in
                                     trace (printf "Patching %s with offset %s" (show currentLine) (show (target - currentLine))) $
                                      applyNth (currentLine `div` 2) bin (backpatchBinary offset))
                                src
                                bps
    return patched


---------------
-- Interface --
---------------

assemble :: Program -> (Binaries, Memory)
assemble p = let (bs, mem) = runState (assembleProgram p >>= backpatch) emptyState
                 bs'       = printBinaries bs
             in
               trace (bs' ++ "\n" ++ show mem) $ (bs, mem)

----------------------------------
-- Backpatch labels into binary --
----------------------------------

-- Compresses a number into 8 bits.
compress2c :: Binary -> Binary
compress2c num =
  let eightBits = num .&. 255
  in
    eightBits

-- Decompresses a number into 16 bits.
decompress2c :: Binary -> Binary
decompress2c num =
  let sixteenBits = num
      sign        = shift (num .&. 128) 8 -- the Sign is on the 8th position from the right.
  in
    num .|. sign



srcMask      = 4032
dstMask      = 63
dstMaskOneOp = 255
operationMask = 61440

-- The offset in a branch instruction is always a signed integer. A branc
-- instruction uses 8 bytes for the operation, and 8 bytes for the offset.
-- Additionally, the bytes of the signed integer are shifted to the right once,
-- because we only have even addresses, we can simply assume that the least
-- significant bit will always be 0.

-- We assume that the numbers in haskell are in 2c.
-- So to compress a number into the 8 bits we do the following.
--
backpatchBinary :: Binary -> Binary -> Binary
backpatchBinary offset b =
  trace (printf "Backpatching binary %s with offset %s" (show b) (show offset)) $
   case op_id of
      0 -> replacerTwoOp operation src dst offset
      1 -> replacerTwoOp operation src dst offset
      2 -> replacerTwoOp operation src dst offset
      3 -> replacerTwoOp operation src dst offset
      4 -> replacerOneOp operation single_dst offset
      5 -> b
  where
    operation  = b .&. operationMask
    op_id      = shift operation (-12)
    src        = b .&. srcMask
    dst        = b .&. dstMask
    single_dst = b .&. dstMaskOneOp

replacerTwoOp :: Binary -> Binary -> Binary -> Binary -> Binary
replacerTwoOp op src dst lbl =
  op .|. newSrc .|. newDst
  where
    newSrc = if src == srcMask then lbl else src
    newDst = if dst == dstMask then lbl else dst

replacerOneOp :: Binary -> Binary -> Binary -> Binary
replacerOneOp op dst lbl =
  trace (printf "Operation: %s Destination: %s Label: %s" (printBinary op) (printBinary dst) (printBinary lbl)) $
    op .|. newDst
  where
    newDst = if dst == dstMask then lbl else dst




-------------
-- Program --
-------------

assembleProgram :: Program -> Compiler Binaries
assembleProgram []     = return []
assembleProgram (i:is) = do
  bs  <- assembleInstruction i
  lineCountInc $ Prelude.length bs * 2
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
assembleOperand (Lbl l) = do
  -- Store the fact we are referencing label l here.
  lc <- lineCount
  setBackpatch l lc
  return (63, Nothing)


inlineOperand :: Operand -> Compiler Binary
inlineOperand (Immed n) = return (fromIntegral n :: Binary)

inlineOperand (Mode0 r) = assembleRegister r

inlineOperand (Mode1 r) = do
  r <- assembleRegister r
  return $ shift 2 3 .|. r

inlineOperand (Mode2 r) = do
  r <- assembleRegister r
  return $ shift 2 3 .|. r

inlineOperand(Lbl l) = do
  lc <- lineCount
  setBackpatch l lc
  return 63

---------------
-- Registers --
---------------

assembleRegister :: Register -> Compiler Binary
assembleRegister R1 = return 1
assembleRegister R2 = return 2
assembleRegister R3 = return 3
assembleRegister R4 = return 4
assembleRegister PC = return 5
