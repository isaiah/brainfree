{-# LANGUAGE NamedFieldPuns #-}

module Expr where

import           Control.Applicative
import           Control.Monad       (foldM)
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

keywords :: [Char]
keywords = "><+-.,[]"

comments :: Parser String
comments = zeroOrMore $ satisfy (\x -> notElem x keywords)

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
           | Loop [Expr]
  deriving (Show, Eq)

-- data Env = Env { cursor :: Int, arr :: Seq Int }
--   deriving Show

type Env = ([Int], [Int])
defaultEnv :: Env
defaultEnv = (repeat 0, repeat 0)

incr :: Env -> Env
incr (ml, m:mr) = (ml, (m+1):mr)

desc :: Env -> Env
desc (ml, m:mr) =
  (ml, (m-1):mr)

forward :: Env -> Env
forward (ml, m:mr) = (m:ml, mr)

backward :: Env -> Env
backward (m:ml, mr) = (ml, m:mr)

puts :: Env -> IO Env
puts env@(ml, m:mr) = do
  putChar $ chr m
  return env

updateEnv :: Env -> Char -> IO Env
updateEnv (ml, _:mr) a =
  return (ml, i:mr)
  where
    f x | x == '\n' = 0
        | otherwise = ord x
    i = f a

enterLoopP :: Env -> Bool
enterLoopP (ml, m:mr) = m /= 0

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

parseExpr :: Parser [Expr]
parseExpr = zeroOrMore (parseAtom <|> parseLoop)
  where
    parseAtom = comments *> (A <$> parseOp) <* comments
    parseLoop = comments *> ((satisfy (== '[')) *> (Loop <$> parseExpr) <* (satisfy (== ']'))) <* comments

evalExpr :: [Expr] -> IO Env
evalExpr exprs =
  foldM go defaultEnv exprs
  where
    go :: Env -> Expr -> IO Env
    go e (A Inc) = return $ incr e
    go e (A Dec) = return $ desc e
    go e (A Forward) = return $ forward e
    go e (A Backward) = return $ backward e
    go e (A Put) =
      puts e
    go e (A Read) = do
      a <- getChar
      updateEnv e a
    go e loop@(Loop ops) = do
      if enterLoopP e
        then do
          ne <- foldM go e ops
          go ne loop
        else return e
