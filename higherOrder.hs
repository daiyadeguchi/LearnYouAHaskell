multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- lambda looks better when it comes to flip function
--flip' :: (a -> b -> c) -> b -> a -> c
--flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
--map' _ [] = []
--map' f (x:xs) = f x : map' f xs
-- Write the same function with foldr
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2) | odd n  = n:chain (n*3 + 1)

--numLongChains :: Int
--numLongChains = length (filter isLong (map chain [1..100]))
--  where isLong xs = length xs > 15

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Both implementation is the same, but second one is not too good
addThree :: (Num a) => a -> a -> a -> a

--addThree x y z = x + y + z
addThree = \x -> \y -> \z -> x + y + z
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

