module Lib where
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Parser

parseFile :: IO ()
parseFile  = do
  a <- getArgs
  case a of
    [str] -> parseFromFile program str >>= either print print
    _     -> error "no file provided"
