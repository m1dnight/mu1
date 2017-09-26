module Lib where
import AST
import Assembler
import Control.Monad
import Parser
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Data.ByteString.Lazy as BS hiding (head, tail, map, foldl)

import Data.Word
import Data.ByteString.Builder

asm :: IO ()
asm =  do
  -- Determine the file to compile
  args <- getArgs
  let file = head args
  let output = head . tail $ args
  -- Parse the file into an AST
  ast <- parseFile file
  mapM_ print ast
  let (bs, mem) = assemble ast
  writeToFile output bs
  return ()

writeToFile :: String -> Binaries -> IO ()
writeToFile f bs = BS.writeFile f binaries
  where
    binaries = foldl (\acc b -> append acc (toLazyByteString . word16BE $ b))  empty bs

parseFile :: String -> IO Program
parseFile f = do
  res <- parseFromFile program f
  case res of
    Left e  -> error $ show e
    Right p -> return p
