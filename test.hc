module Main where

f i j | i == j = 2
f i j = 1


main =
  print $ f 1 2
