module Nfa where

import Data.Set as S

-- provides an implementation of a NFA, using what the paper outlines

type Intermediate a = Set a

type Start a = a

type End a = Set a

type Moves a = Set (Move a)

-- a NFA is 4 things: intermediate states; moves; start state; end states
data NFA a = NFA (Intermediate a) (Moves a) (Start a) (End a)

-- a denotes the state
-- a move is either a character from 1 state to another (can be the same) or an empty move
data Move a = Move a Char a | EmptyMove a a

-- build upon a basic NFA for a simple regex (eg: the literals)
-- for complex regexes involving symbols, expand them out into the literals and build the nfa from there?

-- given the current set of nodes, what states are reachable given the allowable transitions
singleMove :: NFA a -> Char -> Set a -> End a
singleMove = undefined

-- what nodes are reachable from the current node given the allowable transitions?
single :: (Eq a, Ord a) => NFA a -> Char -> a -> End a
-- first, define a helper function to check the transitions for each move
-- then, we filter the moves
single (NFA inter moves start end) c cur =
  -- first get all matching moves
  -- next, for all matching moves, convert to list and extract the end state
  let matchingMoves = S.filter (matching cur c) moves
   in S.map conv matchingMoves
  where
    conv (Move _ _ end) = end
    conv (EmptyMove _ end) = end

matching :: Eq a => a -> Char -> Move a -> Bool
matching a c (Move start trans end) = a == start && c == trans
matching a _ (EmptyMove start end) = a == start