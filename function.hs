doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
                      then x
                      else x*2

boomBangs xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]
--length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- How to define a function
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--factorial :: Integer -> Integer
--factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

-- conditional
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

-- kinda recursive
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

--extract components of triples
first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty list"
                      (x:_) -> x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fat = "overweight"
  | otherwise   = "Giant"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

