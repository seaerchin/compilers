module Parser where

import Control.Monad (join)
import Regex as R
import Text.Parsec
import Text.Parsec.String

data Token = Literal Char | Or | Star | LParen | RParen deriving (Eq)

type Tokens = [Token]

conv :: Char -> Token
conv '*' = Parser.Star
conv '|' = Parser.Or
conv '(' = LParen
conv ')' = RParen
conv c = Parser.Literal c

-- convert a string into a list of tokens
fromString :: String -> Tokens
fromString = map conv

data Tree a = Leaf a | Node (Tree a) a (Tree a)

toAST :: Tokens -> Tree Token
toAST = undefined