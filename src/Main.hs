module Main where

import           Expr
import           Parser

main :: IO ()
main = do
  let raw = ">+++.>,."
  case runParser parseExpr raw of
    Just (expr, _) -> do
      _ <- evalExpr expr
      return ()
    Nothing -> putStrLn "malformatted expression"
