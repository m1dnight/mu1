import Test.HUnit
import Assembler
import Control.Monad.Trans.State.Lazy
import AST

main :: IO ()
main = do
  c <- runTestTT tests
  print c

tests = TestList $ registers ++ operands ++ operators ++ operations ++ programs

---------------
-- Registers --
---------------

registers = [test1, test2, test3, test4, test5]

test1 = TestCase (assertEqual "R1" "0000000000000001" (runAsm' $ assembleRegister R1) )
test2 = TestCase (assertEqual "R2" "0000000000000010" (runAsm' $ assembleRegister R2))
test3 = TestCase (assertEqual "R3" "0000000000000011" (runAsm' $ assembleRegister R3))
test4 = TestCase (assertEqual "R4" "0000000000000100" (runAsm' $ assembleRegister R4))
test5 = TestCase (assertEqual "PC" "0000000000000101" (runAsm' $ assembleRegister PC))


--------------
-- Operands --
--------------

operands = [testo_1, testo_2, testo_3, testo_4, testo_5]

testo_1 = TestCase (assertEqual "Mode0 R1" ("0000000000000001", Nothing) (runAsm'' $ assembleOperand (Mode0 R1)))
testo_2 = TestCase (assertEqual "Mode1 R1" ("0000000000001001", Nothing) (runAsm'' $ assembleOperand (Mode1 R1)))
testo_3 = TestCase (assertEqual "Mode2 R2" ("0000000000010010", Nothing) (runAsm'' $ assembleOperand (Mode2 R2)))

testo_4 = TestCase (assertEqual "Immed 5"      ("0000000000010101", Just "0000000000000001") (runAsm'' $ assembleOperand (Immed 1)))
testo_5 = TestCase (assertEqual "Immed 32767"  ("0000000000010101", Just "0111111111111111") (runAsm'' $ assembleOperand (Immed 32767)))
testo_6 = TestCase (assertEqual "Immed -32768" ("0000000000010101", Just "1111111111111111") (runAsm'' $ assembleOperand (Immed (-32768))))

---------------
-- Operators --
---------------

operators = [testop_1, testop_2, testop_3, testop_4, testop_5]

testop_1 = TestCase (assertEqual "MOV" "0000000000000000" (runAsm' $ assembleOperator MOV))
testop_2 = TestCase (assertEqual "ADD" "0000000000000001" (runAsm' $ assembleOperator ADD))
testop_3 = TestCase (assertEqual "SUB" "0000000000000010" (runAsm' $ assembleOperator SUB))
testop_4 = TestCase (assertEqual "CMP" "0000000000000011" (runAsm' $ assembleOperator CMP))
testop_5 = TestCase (assertEqual "BEQ" "0000000000000100" (runAsm' $ assembleOperator BEQ))

---------------
-- Operation --
---------------

operations = [testopr_1, testopr_2, testopr_3, testopr_4]

testopr_1 = TestCase (assertEqual "MOV R1 R2" ["0000000001000010"] (runAsm $ assembleOperation $ TwoOp MOV (Mode0 R1) (Mode0 R2)))
testopr_2 = TestCase (assertEqual "MOV 5  R2" ["0000010101000010", "0000000000000101"] (runAsm $ assembleOperation $ TwoOp MOV (Immed 5) (Mode0 R2)))
testopr_3 = TestCase (assertEqual "MOV 4  5"  ["0000010101010101", "0000000000000100", "0000000000000101"] (runAsm $ assembleOperation $ TwoOp MOV (Immed 4) (Immed 5)))
testopr_4 = TestCase (assertEqual "BEQ R1 0"  ["0100000001010101", "0000000000000000"] (runAsm $ assembleOperation $ TwoOp BEQ (Mode0 R1) (Immed 0)))
---------------------
-- Entire Programs --
---------------------
programs = [testprog_1, testprog_2]

prog = [
         Op (TwoOp MOV (Immed 5) (Mode0 R1)),
         Op (TwoOp MOV (Immed 2) (Mode0 R2)),
         Op (TwoOp MOV (Mode0 R1) (Mode0 R3)),
         LOp (Labeled "MUL" (TwoOp ADD (Mode0 R3) (Mode0 R1))),
         Op (TwoOp SUB (Immed 1) (Mode0 R2)),
         Op (TwoOp CMP (Mode0 R2) (Immed 0)),
         Op (OneOp BEQ (Immed 4))
       ]

output = [
          "0000010101000001",
          "0000000000000101",
          "0000010101000010",
          "0000000000000010",
          "0000000001000011",
          "0001000011000001",
          "0010010101000010",
          "0000000000000001",
          "0011000010010101",
          "0000000000000000",
          "0100000000000100"
        ]

testprog_1 = TestCase (assertEqual "Program: mul" output (runAsm $ assembleProgram prog))

prog2 =
  [
    Op (TwoOp ADD (Immed 10) (Mode0 R1)),
    LOp (Labeled "LBL" (TwoOp SUB (Immed 1) (Mode0 R1))),
    Op (TwoOp CMP (Mode0 R1) (Immed 0)),
    Op (OneOp BEQ (Immed 10)),
    Op (ZeroOp STOP)
  ]

output2 =
  [
    "0001010101000001",  -- ADD 10 R1
    "0000000000001010",  -- 10
    "0010010101000001",  -- LBL: SUB 1 R1
    "0000000000000001",  -- 1
    "0011000001010101",  -- CMP R1 0
    "0000000000000000",  -- 0
    "0100000000001010",  -- BEQ 10
    "0101000000000000"
  ]

testprog_2 = TestCase (assertEqual "Program: dec loop" output2 (runAsm $ assembleProgram prog2))
-------------
-- Helpers --
-------------

runAsm :: Compiler Binaries -> [String]
runAsm asmr =
  let binaries = evalState asmr emptyState
      str      = map binaryStr binaries
  in
    str

runAsm' :: Compiler Binary -> String
runAsm' asmr =
  let binary = evalState asmr emptyState
      str    = binaryStr binary
  in
    str

runAsm'' :: Compiler (Binary, Maybe Binary) -> (String, Maybe String)
runAsm'' asmr =
  let (binary, opt) = evalState asmr emptyState
      str           = binaryStr binary
      str'          = fmap binaryStr opt
  in
    (str, str')
