module Fibonnaci where

fibonnaci :: Int -> Int
fibonnaci n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise = fibonnaci (n - 1) + fibonnaci (n - 2)
  
