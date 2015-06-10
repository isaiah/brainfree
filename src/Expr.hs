{-# LANGUAGE NamedFieldPuns #-}

module Expr where

import           Control.Applicative
import           Control.Monad       (foldM)
import           Data.Char
import           Data.Sequence hiding (length)
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

-- Brainfuck expression is either operators or loop
data Expr = Inc Int
          | Dec Int
          | Forward Int
          | Backward Int
          | Put Int
          | Read Int
          | Loop [Expr]
  deriving (Eq, Show)

data Env = Env { cursor :: Int, arr :: Seq Int }
  deriving Show

incr :: Env -> Int -> Env
incr env x =
  let i = cursor env
      l = arr env
  in env { arr = update i ((index l i) + x) l }

desc :: Env -> Int -> Env
desc env x =
  let l = arr env
      i = cursor env
  in env { arr = update i ((index l i) - x) l }

puts :: Env -> IO Env
puts env@Env{cursor, arr} = do
  putChar $ chr (index arr cursor)
  return env

updateEnv :: Env -> Char -> IO Env
updateEnv e@Env { arr, cursor } a =
  return e{ arr = update cursor i arr }
  where
    f x | x == '\n' = 0
        | otherwise = ord x
    i = f a

enterLoopP :: Env -> Bool
enterLoopP Env { arr, cursor } =
  cursor >= 0 && index arr cursor /= 0

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
    parseLoop = (satisfy (== '[')) *> (Loop <$> parseExpr) <* (satisfy (== ']'))

evalExpr :: [Expr] -> IO Env
evalExpr exprs =
  foldM go env exprs
  where
    env = Env 0 (replicate 3000 0)
    go :: Env -> Expr -> IO Env
    go env (Inc i) = return $ incr env i
    go env (Dec i) = return $ desc env i
    go e (Forward i) = return $ e { cursor = (cursor e) + i }
    go e (Backward i) = return $ e { cursor = (cursor e) - i }
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
