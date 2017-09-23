module Lib where
import AST
import Assembler
import Control.Monad
import Parser
import System.Environment
import Text.Parsec
import Text.Parsec.String


runAssembler :: IO ()
runAssembler = do
  p <- parseFile
  b <- assemble p
  print p
  print b
  return ()



parseFile :: IO Program 
parseFile = do
  a <- getArgs
  case a of
    [as] -> do res <- parseFromFile program as
               case res of
                 Left e  -> error $ show e
                 Right p -> return p
    _    -> error "Expected exactly one argument"

assemble :: Program -> IO Int
assemble _ = return 5
               
              
