module Kleene where

import DFA
import RegExp
import qualified Data.Map as Map
import qualified Data.Set as Set

-- _0 = Symbol '0'
-- _1 = Symbol '1'
--
-- dfa :: DFA
-- dfa = DFA
--    { states = Set.fromList [1, 2]
--    , symbols = Set.fromList ['0', '1']
--    , delta = Map.fromList
--            [ ((1, '1'), 1)
--            , ((1, '0'), 2)
--            , ((2, '1'), 2)
--            , ((2, '0'), 2) ]
--    , start = 1
--    , accepting = Set.fromList [2] }


kleene' i j 0 dfa | i == j =
  let
    transitions = filter (\((i', _), j') -> i == i' && j == j')  (Map.toList $ delta dfa)
    characters = map (\((_, transitionChar), _) -> transitionChar) transitions
    symbols = map Symbol characters
  in
    case symbols of
      [] ->
        Epsilon

      symbols ->
        foldr Plus Epsilon symbols


kleene' i j 0 dfa =
  let
    transitions = filter ( \((i', _), j') -> i == i' && j == j' )  (Map.toList (delta dfa))
    characters = map ( \((_, transitionChar), _) -> transitionChar) transitions
    symbols = map Symbol characters
  in
    case symbols of
      [] ->
        Empty

      [symbol] ->
        symbol

      (symbol:symbols) ->
        foldr Plus symbol symbols


kleene' i j k dfa =
  (Plus
    (kleene' i j (k-1) dfa)
    (Dot
      (Dot
        (kleene' i k (k-1) dfa)
        (Star (kleene' k k (k-1) dfa))
      )
      (kleene' k j (k-1) dfa)
    )
  )


-- kleene' :: Int -> Int -> Int -> DFA -> RegExp
-- kleene' i j 0 dfa | ((i, *** ), j) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _0
--                   | ((1,'1'),1) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _1
--                   | otherwise = Epsilon


--
-- kleene' :: Int -> Int -> Int -> DFA -> RegExp
-- kleene' i j 0 dfa | ((i, *** ), j) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _0
--                   | ((1,'1'),1) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _1
--                   | otherwise = Epsilon
--
--
--
-- kleene' 1 2 0 dfa | ((1,'0'),2) `elem` ( Map.toList (delta dfa)) = _0
--                   | ((1,'1'),2) `elem` ( Map.toList (delta dfa)) = _1
--                   | otherwise = Empty
--
-- dfaStates dfa = Set.toList (states dfa)
-- n dfa = length $ dfaStates dfa
--
--
--
-- kleene' i j 0 dfa | ((i,_),j) `elem` ( Map.toList (delta dfa)) = _
-- 		 		  | otherwise = Empty
-- 		 		  where i j = [1..n]
--
--
--
-- extractStates :: DFA -> [(Int, Int, Int)]
-- extractStates dfa =
--   [ (i, j, k) |
--            k <- [0..(n dfa)],
--            i <- (dfaStates dfa),
--            j <- (dfaStates dfa) ]
--
--
-- kleene :: DFA ->  RegExp
-- kleene dfa =
--
--   kleene' i j k
--
--
--
-- kleene i j k = Plus (kleene i j (k-1)) (Dot (Dot (kleene i k k-1) (kleene k k k-1)) kleene k j k-1)
