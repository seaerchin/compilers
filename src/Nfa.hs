module Nfa where

-- containers is annoying b/c set doesn't have inbuilt typeclasses but provides functionality in its own lib
-- means everytime i have to check the hackage docs to see if it exists instead of trusting compiler when it says that it doesn't
import Data.Set as S
import Parser (PostFix, RPN, Reg (RegLiteral, RegStar), parse)
import Regex (fromParsed)

-- with reference from: https://www.cs.kent.ac.uk/people/staff/sjt/craft2e/regExp.pdf
-- this is a crude attempt to translate a regex into a nfa
-- and subsequently, convert the nfa into a dfa
-- provides an implementation of a NFA, using what the paper outlines

type Intermediate a = Set a

type Start a = a

type End a = Set a

type Moves a = Set (Move a)

-- a NFA is 4 things: intermediate states; moves; start state; end states
data NFA a = NFA (Intermediate a) (Moves a) (Start a) (End a) deriving (Show)

-- a denotes the state
-- a move is either a character from 1 state to another (can be the same) or an empty move
data Move a = Move a Char a | EmptyMove a a deriving (Ord, Eq, Show)

-- build upon a basic NFA for a simple regex (eg: the literals)
-- for complex regexes involving symbols, expand them out into the literals and build the nfa from there?

-- given the current set of nodes, what states are reachable given the allowable transitions
singleMove :: (Ord a, Eq a) => NFA a -> Char -> Set a -> End a
-- map single across a
-- fold using union into a single set and return
-- also, this should really be >>= for monad wtf
singleMove nfa c start = S.foldr' S.union S.empty $ S.map (single nfa c) start

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

-- given an NFA and a string, finds the possible nodes that can be reached from the NFA
-- this set of end nodes is distinct (and can possibly overlap fully) w/ the NFA's end nodes
transition :: Ord a => NFA a -> String -> End a
transition nfa@(NFA inter moves start end) = Prelude.foldr (singleTransition nfa) (singleton start)

-- transits from a start state -> possible end states
-- iterate through list of all moves and start state
-- next, find those moves where the start state is equal to our start state
-- and where the character is equal to what we passed in
singleTransition :: Ord a => NFA a -> Char -> Set a -> Set a
singleTransition (NFA _ moves _ _) c start = fromList [end | t <- toList start, Move z char end <- toList moves, z == t, c == char]

-- had to specify a concrete type :(
formNFA :: Reg -> NFA Int
formNFA (RegLiteral c) =
  let states = S.fromList [0 .. 1]
      moves =
        S.fromList
          [ Move 0 c 1
          ]
      start = 0
      end = S.fromList [1]
   in NFA states moves start end
formNFA (RegStar exp) =
  let baseNFA = formNFA exp
   in -- for our new NFA,
      -- we must allow empty moves from new start -> old start
      -- we must allow empty moves from old end -> old start
      -- we must allow empty moves from old end -> new end
      -- lastly, allow empty moves from new start -> new end
      case baseNFA of
        NFA states moves start end ->
          let newStart = start - 1
              -- just set the new end state to be 1 bigger than the old
              newEnd = findMax end + 1
              newStartOldStart = formEmptyMoves newStart [start]
              -- need to fold below 2
              oldEndOldStart = reduceList $ Prelude.map (\oldEnd -> formEmptyMoves oldEnd [start]) (S.toList end)
              oldEndNewEnd = reduceList $ Prelude.map (\oldEnd -> formEmptyMoves oldEnd [newEnd]) (S.toList end)
              newStartnewEnd = formEmptyMoves newStart [newEnd]
           in NFA (S.singleton start `S.union` end `S.union` S.singleton newEnd `S.union` S.singleton newStart) (S.unions [newStartOldStart, newStartnewEnd, oldEndOldStart, oldEndNewEnd]) newStart (S.singleton newEnd)
formNFA _ = undefined

formEmptyMoves :: (Ord a) => a -> [a] -> Moves a
formEmptyMoves start = S.fromList . Prelude.map (EmptyMove start)

reduceList :: (Foldable f, Ord a) => f (Moves a) -> Moves a
reduceList = Prelude.foldr S.union S.empty

mkNFA = print . formNFA . fromParsed . Parser.parse