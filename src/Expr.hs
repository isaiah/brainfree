{-# LANGUAGE NamedFieldPuns #-}

module Expr where

import           Control.Applicative
import           Control.Monad       (foldM)
import           Data.Char
import           Data.Sequence       hiding (length)
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

keywords :: Seq Char
keywords = fromList "><+-.,[]"

comments :: Parser String
comments = zeroOrMore $ satisfy (\x -> notElem x keywords)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- Brainfuck expression is either operators or loop
data Expr = Inc Int
          | Dec Int
          | Forward Int
          | Backward Int
          | Put Int
          | Read Int
          | Loop [Expr]
  deriving (Eq, Show)

type Env = ([Int], [Int])

defaultEnv :: Env
defaultEnv = (repeat 0, repeat 0)

incr :: Env -> Int -> Env
incr (ml, m:mr) x = (ml, (m+x):mr)

desc :: Env -> Int -> Env
desc (ml, m:mr) x =
  (ml, (m-x):mr)

forward :: Env -> Int -> Env
forward e 0 = e
forward (ml, m:mr) x = forward (m:ml, mr) (x - 1)

backward :: Env -> Int -> Env
backward e 0 = e
backward (m:ml, mr) x = backward (ml, m:mr) (x - 1)

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

-- parseOp :: Parser Expr
-- parseOp = Parser f
--   where
--     f :: String -> Maybe (Expr, String)
--     f [] = Nothing
--     f (x:xs) =
--       case x of
--         '+' -> Just (Inc 1, xs)
--         '-' -> Just (Dec 1, xs)
--         '>' -> Just (Forward, xs)
--         '<' -> Just (Backward, xs)
--         '.' -> Just (Put, xs)
--         ',' -> Just (Read, xs)
--         _ -> Nothing

-- parses continous operators
parseOp :: (Int -> Expr) -> Char -> Parser Expr
parseOp ctor op = (\l -> ctor $ length l) <$> oneOrMore (satisfy (== op))

-- parses continuous increase
parseInc = parseOp Inc '+'
parseDec = parseOp Dec '-'
parseForward = parseOp Forward '>'
parseBackward = parseOp Backward '<'
parsePut = parseOp Put '.'
parseRead = parseOp Read ','

parseExpr :: Parser [Expr]
parseExpr = zeroOrMore $ comments *> (parseAtom <|> parseLoop) <* comments
  where
    parseAtom = parseInc <|> parseDec <|> parseForward <|> parseBackward <|> parsePut <|> parseRead
    parseLoop = satisfy (== '[') *> (Loop <$> parseExpr) <* satisfy (== ']')

evalExpr :: [Expr] -> IO Env
evalExpr =
  foldM go defaultEnv
  where
    go :: Env -> Expr -> IO Env
    go env (Inc i) = return $ incr env i
    go env (Dec i) = return $ desc env i
    go e (Forward i) = return $ forward e i
    go e (Backward i) = return $ backward e i
    go e (Put _) =
      puts e
    go e (Read _) = do
      a <- getChar
      updateEnv e a
    go e loop@(Loop ops) = do
      if enterLoopP e
        then do
          ne <- foldM go e ops
          go ne loop
        else return e
