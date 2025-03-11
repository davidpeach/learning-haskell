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

fac' :: Integer -> Integer
fac' 0 = 1
fac' x = fac' (x-1) * x

sum' :: Integer -> Integer
sum' 0 = 0
sum' x = sum' (x-1) + x

fst' :: (a,b,c) -> a
fst' (a, _, _) = a

snd' :: (a,b,c) -> b
snd' (_, b, _) = b

lst' :: (a,b,c) -> c
lst' (_, _, c) = c

head' :: [x] -> x
head' [] = error "NOPE"
head' (x:xs) = x

age' :: Integer -> String
age' x
    | x < 13 = "Youngen'"
    | x < 18 = "Not adult yet"
    | x < 60 = "Adult"
    | otherwise = "Older"

max' :: (Ord x) => [x] -> x
max' [] = error "Empty List"
max' [x] = x
max' (x: xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = max' xs

rep :: (Num x, Ord x) => x -> y -> [y]
rep n x
    | n <= 0 = []
    | otherwise = x:rep (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' [a | a <- xs, a <= x]
        biggerSorted = quicksort' [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

largestDiv :: (Integral a) => a
largestDiv  = head (filter p [10000,9999..])
    where p x = mod x 3829 == 0

smallOddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
smallOddSquares' = sum ( takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain  (div n 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

qs' :: Ord a => [a] -> [a]
qs' [] = []
qs' (x:xs) =
    let smallerSorted = qs' (filter' (<= x) xs)
        biggerSorted = qs' (filter' (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
