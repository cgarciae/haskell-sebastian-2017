module Kleene where

import DFA
import RegExp
import qualified Data.Map as Map
import qualified Data.Set as Set

_0 = Symbol '0'
_1 = Symbol '1'

dfa :: DFA
dfa = DFA
   { states = Set.fromList [1, 2]
   , symbols = Set.fromList ['0', '1']
   , delta = Map.fromList
           [ ((1, '1'), 1)
           , ((1, '0'), 2)
           , ((2, '1'), 2)
           , ((2, '0'), 2) ]
   , start = 1
   , accepting = Set.fromList [2] }

kleene' :: Int -> Int -> Int -> DFA -> RegExp
kleene' 1 1 0 dfa | ((1,'0'),1) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _0 
                  | ((1,'1'),1) `elem` ( Map.toList (delta dfa)) = Plus Epsilon _1 
                  | otherwise = Epsilon

kleene' 1 2 0 dfa | ((1,'0'),2) `elem` ( Map.toList (delta dfa)) = _0 
                  | ((1,'1'),2) `elem` ( Map.toList (delta dfa)) = _1 
                  | otherwise = Empty

a = Set.toList (states dfa)
n = length a 


{-
kleene' i j 0 dfa | ((i,_),j) `elem` ( Map.toList (delta dfa)) = _ 
		 		  | otherwise = Empty
		 		  where i j = [1..n]
-}




