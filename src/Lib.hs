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
  let (b, _m) = assemble p
  let bs = printBinaries b
  putStrLn "AST\n"
  print p
  putStrLn "Program binary\n"
  putStrLn bs
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
