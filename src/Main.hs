module Main where

data Op = Inc
        | Dec
        | Forward
        | Backward
        | Put
        | Read
        | LStart
        | LEnd

data Atom = A Op
          | L [Atom]

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

bfParser :: Parser Atom

main = IO ()
main = do
  putStrLn "============Brain Free================="
