doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNum x = if x > 100
then x
else x*2

boomBangs xs = [if x < 10 then "BOOM" else "BANG"| x <- xs, odd x]

onlyCaps :: [Char] -> [Char]
onlyCaps st = [c | c <- st, elem c ['A'..'Z']]

len' st = sum [1 | c <- st, (elem c [' ']) == False]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
