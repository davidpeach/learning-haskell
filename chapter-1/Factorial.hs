module Factorial where

-- My own implementation after reading chapter 1 of "Effective Haskell"
factorial num total = 
  if num < 2
  then show total
  else
    let nextNum = (num - 1)
        curTotal = 
          if total == 1
          then num * nextNum
          else total * nextNum
    in factorial nextNum curTotal

-- Found online after solving with above
factorial' num
  | num < 2 = 1
  | otherwise = num * factorial' (num - 1)
