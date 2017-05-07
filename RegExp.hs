module RegExp where

data RegExp = Empty
            | Epsilon
            | Symbol Char
            | Star RegExp
            | Plus RegExp RegExp
            | Dot  RegExp RegExp