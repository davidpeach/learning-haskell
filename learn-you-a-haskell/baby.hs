doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleIfBig x
  | x < 100 = x
  | otherwise = doubleMe x

fib num
  | num <= 0 = 0
  | num == 1 = 1
  | otherwise = fib (num - 2) + fib (num - 1)

pwr :: (Eq a, Num a) => a -> a -> a
pwr n p
  | p == 1 = n
  | otherwise = n * pwr n (p - 1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallSort = quicksort [a | a <- xs, a <= x]
      bigSort = quicksort [a | a <- xs, a > x]
   in smallSort ++ [x] ++ bigSort

filterCapitals xs = [x | x <- xs, x `elem` ['a' .. 'z']]

filterNumbers s = [x | x <- s, x `elem` [1 .. 10]]

filterLetters s = [x | x <- s, x `elem` ['a' .. 'z']]
