{-# LANGUAGE LambdaCase #-}

module Nfa where

-- containers is annoying b/c set doesn't have inbuilt typeclasses but provides functionality in its own lib
-- means everytime i have to check the hackage docs to see if it exists instead of trusting compiler when it says that it doesn't

import Data.Char (ord)
import Data.Set as S
import Parser (PostFix, RPN, Reg (RegAnd, RegEmpty, RegLiteral, RegOr, RegStar), parse)
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

-- this is formally the same thing as an nfa but without empty transitions
-- we could, alternatively, define 2 separate data types to model this
-- this is less ideal but less work
newtype DFA a = DFA {getDFA :: NFA a}

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
  let chrRep = ord c
      start = chrRep - 1
      states = S.fromList [start, chrRep]
      moves =
        S.fromList
          [ Move start c chrRep
          ]
      end = S.fromList [chrRep]
   in NFA states moves start end
formNFA RegEmpty =
  let start = 0
      end = 1
      states = S.fromList [start, end]
      moves = S.singleton (EmptyMove start end)
   in NFA states moves start (S.singleton end)
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
           in NFA (S.singleton start `S.union` end `S.union` S.singleton newEnd `S.union` S.singleton newStart) (S.unions [newStartOldStart, newStartnewEnd, oldEndOldStart, oldEndNewEnd, moves]) newStart (S.singleton newEnd)
-- we add an artificial start (s)
-- and an artificial end (e)
-- next, allow empty transition from s -> old start
-- and empty transition from old end -> e
formNFA (RegOr b a) =
  let nfaA = formNFA a
      nfaB = formNFA b
      newStart = minimum (Prelude.map getStart [nfaA, nfaB]) - 1
      newEnd = S.findMax (S.unions $ Prelude.map getEnd [nfaA, nfaB]) + 1
      newStartOldStart = formEmptyMoves newStart [getStart nfaA, getStart nfaB]
      oldEndNewEnd = reduceList $ Prelude.map (`formEmptyMoves` [newEnd]) (S.toList (getEnd nfaA `S.union` getEnd nfaB))
      newStates = getStates nfaA `S.union` getStates nfaB `S.union` S.singleton newStart `S.union` S.singleton newEnd
      newMoves = S.unions [reduceList $ Prelude.map getMoves [nfaA, nfaB], newStartOldStart, oldEndNewEnd]
   in NFA newStates newMoves newStart (S.singleton newEnd)
formNFA (RegAnd b a) =
  -- for and, we just connect a's end state to b's start state using empty transitions
  let nfaA = formNFA a
      nfaB = formNFA b
      endA = getEnd nfaA
      startB = getStart nfaB
      endAStartB = reduceList $ Prelude.map (\startA -> formEmptyMoves startA [startB]) (S.toList endA)
      newStates = getStates nfaA `S.union` getStates nfaB
      newMoves = S.unions [getMoves nfaA, getMoves nfaB, endAStartB]
   in NFA newStates newMoves (getStart nfaA) (getEnd nfaB)

-- this is the subset construction algo
-- all empty transitions reachable from the first state are part of the same "state cluster"
-- first, maintain a list, L, of states, S, which is initialized to start state of N (where N is the nfa)
-- next, for all s in L,
toDFA :: (Ord a) => NFA a -> DFA a
toDFA nfa =
  let start = getEmptyTransitions (getStates nfa) nfa
      machine = NFA (S.singleton start) S.empty start S.empty
   in constructSubset start []
  where
    -- each state in the stack is a state of the DFA
    constructSubset :: Intermediate a -> [Intermediate a] -> DFA a
    constructSubset state stack = undefined

-- gets all empty transitions
getEmptyTransitions :: (Ord a) => Intermediate a -> NFA a -> Intermediate a
getEmptyTransitions states nfa =
  let emptyMoves = S.filter isEmptyMove (getMoves nfa)
      outgoingTransitions =
        S.unions $
          S.map
            -- filter empty moves
            ( \s ->
                S.filter
                  ( \case
                      Move {} -> False
                      EmptyMove s _ -> True
                  )
                  emptyMoves
            )
            states
      outgoingSets = S.map (\(EmptyMove a b) -> b) outgoingTransitions
   in S.filter (`S.member` outgoingSets) (getStates nfa)

isEmptyMove :: Move a -> Bool
isEmptyMove (EmptyMove _ _) = True
isEmptyMove _ = False

-- given a start and end state, make moves going from start to end using the empty transition
formEmptyMoves :: (Ord a) => a -> [a] -> Moves a
formEmptyMoves start = S.fromList . Prelude.map (EmptyMove start)

reduceList :: (Foldable f, Ord a) => f (Moves a) -> Moves a
reduceList = Prelude.foldr S.union S.empty

mkNFA = print . formNFA . fromParsed . Parser.parse

-- utility functions for extraction of parts of the NFA
getStart :: NFA a -> Start a
getStart (NFA _ _ s _) = s

getMoves :: NFA a -> Moves a
getMoves (NFA _ moves _ _) = moves

getEnd :: NFA a -> End a
getEnd (NFA _ _ _ end) = end

getStates :: NFA a -> Intermediate a
getStates (NFA st _ _ _) = st