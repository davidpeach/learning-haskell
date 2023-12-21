module Main where
import Data.Attoparsec.ByteString.Char8 (number)

-- makeGreeting salutation person = salutation <> " " <> person

-- makeGreeting' salutation = ((salutation <> " ") <>)
-- makeGreeting' salutation = (<>) (salutation <> " ")
-- makeGreeting' = (<>) . (\salutation -> salutation <> " ")
-- makeGreeting' = (<>) . (<> " ")

-- makeGreeting salutation person = 
--   let starting = salutation <> " "
--   in starting <> person
--
-- extGreet person = 
--   let joinIt a b = a <> " -- " <> b
--       helloAndBye hello bye = 
--         let hello' = makeGreeting hello person
--             bye' = makeGreeting bye person
--         in joinIt hello' bye'
--   in helloAndBye "Hello" "Bye"


zzz num
  | num > 3 = pred <> " than 3"
  | num > 10 = pred <> "Bigger than 10"
  | otherwise = "Dunno"
  where pred = "Bigger!"

fizzbuzz num
  | 0 == num `rem` 15 = "FizzBuzz"
  | 0 == num `rem` 3 = "Fizz"
  | 0 == num `rem` 5 = "Buzz"
  | otherwise = show num

main = print "Nothing to show yet"
