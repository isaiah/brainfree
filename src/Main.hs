module Main where

import           Expr
import           Parser
import           System.Environment
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  raw <- readFile file
  -- disable buffering
  hSetBuffering stdout NoBuffering
  case runParser parseExpr raw of
    Just (expr, "") -> do
      _ <- evalExpr expr
      return ()
    _ -> putStrLn "syntax error"
