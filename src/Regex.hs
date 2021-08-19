module Regex where

import Control.Applicative
import Data.Char (toUpper)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char (letter)

-- with reference from: https://www.cs.kent.ac.uk/people/staff/sjt/craft2e/regExp.pdf
-- this is a crude attempt to translate a regex into a nfa
-- and subsequently, convert the nfa into a dfa

-- Describes the set of regex expressions
-- not supporting anymore for simplicity
-- TODO: extend this to include choice potentially
data Reg = Empty | Literal Char | Or Reg Reg | And Reg Reg | Star Reg deriving (Eq)

-- prints all the elements of the given regex
literals :: Reg -> String
-- placeholder since we want to signal the presence of an empty string
literals Regex.Empty = "~"
literals (Literal s) = [s]
literals (Or a b) = literals a ++ " | " ++ literals b
literals (And a b) = literals a ++ " . " ++ literals b
literals (Star reg) = literals reg ++ "*"

-- checks if the string matches the given regex
matches :: Reg -> String -> Bool
matches Regex.Empty s = null s
matches (Literal c) s = [c] == s
-- either the first regexp matches or second regexp matches or false
matches (Or a b) s = matches a s || matches b s
-- generate all sublists from a b and check if any match
matches (And a b) s = or [matches a s1 && matches b s2 | (s1, s2) <- splits s]
matches (Star reg) s = matches Regex.Empty s || or [matches reg s1 && matches (Star reg) s2 | (s1, s2) <- frontSplits s]

splits :: [a] -> [([a], [a])]
splits s = [splitAt n s | n <- [0 .. length s]]

frontSplits :: [a] -> [([a], [a])]
frontSplits = tail . splits
