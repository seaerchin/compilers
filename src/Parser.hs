module Parser where

import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String

-- Describes the set of regex expressions
-- not supporting anymore for simplicity
-- TODO: extend this to include choice potentially
data Reg = RegEmpty | RegLiteral Char | RegOr Reg Reg | RegAnd Reg Reg | RegStar Reg deriving (Eq)

data Token = Literal Char | Or | Star | LParen | RParen deriving (Eq, Show)

-- not sure how to represent this elegantly
data Parsed = Tok Token | And deriving (Eq, Show)

type Tokens = [Token]

type ParsedToks = [Parsed]

conv :: Char -> Token
conv '*' = Parser.Star
conv '|' = Parser.Or
conv '(' = LParen
conv ')' = RParen
conv c = Parser.Literal c

-- convert a string into a list of tokens
fromString :: String -> Tokens
fromString = map conv

-- an implementation of the shunting yard algorithm in haskell
-- refer to: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
-- note caveats:
-- 1. shunting yard requires operator precedence/associativity
-- because we support a limited grammar, this is explicitly ignored
-- shunting yard has support for functions -> NOT IMPLEMENTED (since no functions)
parseRegex :: ParsedToks -> ParsedToks
parseRegex = f [] []
  where
    -- f takes q stack input
    -- if there is no input and nothing on stack, we are done
    f q [] [] = q
    -- we have items on the stack
    -- pop stack onto q and continue
    f q stack [] = f (q ++ reverse stack) [] []
    f q stack (tok : tokens) =
      case tok of
        t@(Tok (Parser.Literal c)) -> f (q ++ [t]) stack tokens
        t@(Tok LParen) -> f q (stack ++ [t]) tokens
        Tok RParen ->
          let (newQ, newStack) = fixRParen q stack
           in f newQ newStack tokens
        -- either Or or Star
        op ->
          let (newQ, newStack) = fixUp q stack
           in f newQ (newStack ++ [op]) tokens
    fixUp q [] = (q, [])
    fixUp q stack =
      let topOp = last stack
       in case topOp of
            Tok LParen -> (q, stack)
            _ -> (q ++ [topOp], init stack)
    fixRParen q [] = (q, [])
    fixRParen q stack =
      let topOp = last stack
       in case topOp of
            Tok LParen -> (q, init stack)
            _ -> fixRParen (q ++ [topOp]) (init stack)

-- basically, this adds a new (explicit) concat operator to the regex
-- ie, ab -> a . b
-- for all regexes of the form AB, where A and B are also regexes,
-- we only add the . if A is a valid regex by itself.
-- this implies that partial regexes (ie, A | (some part)) or (A .. some part <-- NO CLOSING
-- both are not of the form AB and cannot be accepted.
-- since our grammar is limited to * . | ( ), we can simplify.
insertAnd :: Tokens -> ParsedToks
insertAnd = f []
  where
    f output [] = output
    -- only 1 element in the list
    f output [tok] = f (output ++ [Tok tok]) []
    -- at least 2 items in the list
    f output (tok : next : tokens) =
      let newTok = Tok tok
          newOutput = output ++ [newTok]
       in case tok of
            -- operators awaiting application - add to output and skip
            LParen -> f newOutput (next : tokens)
            Parser.Or -> f newOutput (next : tokens)
            -- valid regexes -> lookahead and decide if we need to add AND operator
            _ -> case next of
              Parser.Literal c -> f (newOutput ++ [Parser.And]) (next : tokens)
              _ -> f newOutput (next : tokens)

-- io testing functions because lazy to write test suites since it's provably correct.
testParse :: IO ()
testParse = do
  s <- getLine
  let toks = (parseRegex . insertAnd . fromString) s
  print toks

testInsert :: IO ()
testInsert = do
  s <- getLine
  print $ (insertAnd . fromString) s