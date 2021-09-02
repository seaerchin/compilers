module Regex where

import Data.Char (toUpper)
import Data.List
import Parser (Parsed (And, Tok), ParsedToks, PostFix (PAnd, PLiteral, POr, PStar), RPN, Reg (RegAnd, RegEmpty, RegLiteral, RegOr, RegStar), Token (Or, Star), parse)
import Text.Parsec
import Text.Parsec.Char (letter)

-- with reference from: https://www.cs.kent.ac.uk/people/staff/sjt/craft2e/regExp.pdf
-- this is a crude attempt to translate a regex into a nfa
-- and subsequently, convert the nfa into a dfa

-- prints all the elements of the given regex
literals :: Reg -> String
-- placeholder since we want to signal the presence of an RegEmpty string
literals RegEmpty = "~"
literals (RegLiteral s) = [s]
literals (RegOr a b) = "(" ++ literals a ++ "|" ++ literals b ++ ")"
literals (RegAnd a b) = literals a ++ literals b
literals (RegStar reg) = "(" ++ literals reg ++ ")" ++ "*"

-- checks if the string matches the given regex
matches :: Reg -> String -> Bool
matches RegEmpty s = null s
matches (RegLiteral c) s = [c] == s
-- either the first regexp matches or second regexp matches or false
matches (RegOr a b) s = matches a s || matches b s
-- generate all sublists from a b and check if any match
matches (RegAnd a b) s = or [matches a s1 && matches b s2 | (s1, s2) <- splits s]
matches (RegStar reg) s = matches RegEmpty s || or [matches reg s1 && matches (Parser.RegStar reg) s2 | (s1, s2) <- frontSplits s]

splits :: [a] -> [([a], [a])]
splits s = [splitAt n s | n <- [0 .. length s]]

frontSplits :: [a] -> [([a], [a])]
frontSplits = tail . splits

-- we support: * c | .
-- because the only unary operator here is *
-- we can simplify the structure of the parsed tokens
-- it is either of: Expr Expr Op
-- or Exp Op (this is explicitly *)
-- hence, we just keep reading
fromParsed :: RPN -> Reg
fromParsed expr = head $ f [] expr
  where
    f stack [] = stack
    f stack (op : rest) = case op of
      PLiteral c -> f (stack ++ [RegLiteral c]) rest
      POr ->
        let expr = RegOr lastElem secondLast
         in f (rem ++ [expr]) rest
      PStar ->
        f (init stack ++ [RegStar lastElem]) rest
      PAnd ->
        let expr = RegAnd lastElem secondLast
         in f (rem ++ [expr]) rest
      where
        lastElem = last stack
        secondLast = (last . init) stack
        rem = (init . init) stack

-- note case
-- (ab|cd)e -> (ab | c)de
test = print . literals . fromParsed . Parser.parse
