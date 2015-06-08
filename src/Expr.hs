{-# LANGUAGE NamedFieldPuns #-}

module Expr where

import           Control.Applicative
import           Data.Char
import           Data.Sequence
import           Parser
import           Prelude             hiding (replicate)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Eq, Show)

-- Brainfuck operators
data Op = Inc
        | Dec
        | Forward
        | Backward
        | Put
        | Read
  deriving (Eq, Show)

-- An S-expression is either an atom, or a list of S-expressions.
data Expr = A Op
           | Comb [Expr]
  deriving (Show, Eq)

data Env = Env { cursor :: Int, arr :: Seq Int }
  deriving Show

incr :: Env -> Env
incr env =
  let i = cursor env
      l = arr env
  in env { arr = update i ((index l i) + 1) l }

desc :: Env -> Env
desc env =
  let l = arr env
      i = cursor env
  in env { arr = update i ((index l i) - 1) l }

puts :: Env -> IO Env
puts env@Env{cursor, arr} = do
  print $ chr (index arr cursor)
  return env

updateEnv :: Env -> Char -> IO Env
updateEnv e@Env { arr, cursor } a =
  return e{ arr = update cursor i arr }
  where
    f x = if isDigit x
            then digitToInt x
            else ord x
    i = f a

parseOp :: Parser Op
parseOp = Parser f
  where
    f :: String -> Maybe (Op, String)
    f [] = Nothing
    f (x:xs) =
      case x of
        '+' -> Just (Inc, xs)
        '-' -> Just (Dec, xs)
        '>' -> Just (Forward, xs)
        '<' -> Just (Backward, xs)
        '.' -> Just (Put, xs)
        ',' -> Just (Read, xs)
        _ -> Nothing

parseExpr :: Parser Expr
parseExpr = spaces *> (Comb <$> (zeroOrMore parseAtom) <|> ((satisfy (== '[')) *> parseComb <* (satisfy (== ']')))) <* spaces
  where
    parseAtom = A <$> parseOp
    parseComb = Comb <$> (oneOrMore parseExpr)

evalExpr :: Expr -> IO Env
evalExpr expr =
  go env expr
  where
    env = Env 0 (replicate 3000 0)
    go :: Env -> Expr -> IO Env
    go env (A Inc) = return $ incr env
    go env (A Dec) = return $ desc env
    go e (A Forward) = return $ e{cursor = (cursor e) + 1}
    go e (A Backward) = return $ e{cursor = (cursor e) - 1}
    go e (A Put) =
      puts e
    go e (A Read) = do
      a <- getChar
      updateEnv e a
    go e (Comb (x:xs)) = do
      ne <- go e x
      go ne (Comb xs)
    go e _ =
      return e
