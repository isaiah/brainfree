module Main where

import           Expr
import           Parser
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  raw <- readFile file
  case runParser parseExpr raw of
    Just (expr, rst) -> do
      print rst
      _ <- evalExpr expr
      return ()
    Nothing -> putStrLn "malformatted expression"
