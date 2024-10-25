doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

uc' cs = [ c | c <- cs, c `elem` ['A'..'Z']]

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

thd' :: (a, b, c) -> c
thd' (_, _, z) = z

head' :: [a] -> a
head' [] = error "Empty list given"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "Empty list given"
tail' (_:xs) = xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Average"
  | bmi <= fat = "Overwright"
  | otherwise = "NOT SURE"
  where bmi = weight / (height ^ 2)
        (skinny, normal, fat) = (18.5, 25, 30)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l]
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
  where bmi weight height = weight / (height ^ 2)
