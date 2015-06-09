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
    Just (expr, "") -> do
      _ <- evalExpr expr
      return ()
    _ -> putStrLn "syntax error"
