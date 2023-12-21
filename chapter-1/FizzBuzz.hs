module FizzBuzz where

fizzBuzzFor num
  | 0 == num `rem` 15 = "FizzBuzz"
  | 0 == num `rem` 3 = "Fizz"
  | 0 == num `rem` 5 = "Buzz"
  | otherwise = show num

naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString = 
  if curNum > fizzBuzzCount
  then fizzBuzzString
  else
    let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
        nextNumber = curNum + 1
    in naiveFizzBuzz fizzBuzzCount nextNumber nextFizzBuzzString
