module DFA where

import Data.Set
import Data.Map

type Delta = Map (Int, Char) Int

data DFA = DFA
    { states :: Set Int
    , symbols :: Set Char
    , delta :: Delta
    , start :: Int
    , accepting :: Set Int
    }

    