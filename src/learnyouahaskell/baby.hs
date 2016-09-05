data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Read, Show)


doubleMe x = x + x

doubleUs x y = 2 * x + 2 * y

doubleSmallNumber x =
    if x > 100
        then x
        else x * 2

conanO'Brien = "It's a-me, Conan O'Brien!"

triangles =
    [ (a, b, c) | c <- [1..10],
                  b <- [1..c],
                  a <- [1..b],
                  a**2 + b**2 == c**2,
                  a + b + c == 24 ]

factorial :: Integer -> Integer
factorial n = product [1..n]

-- show True >> "True"
-- read "True" >> True


head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

sum' :: (Num a) => [a] -> a
sum' xs =
    foldl (\acc x -> acc + x) 0 xs


sqrtSums = length ((takeWhile (<=1000)) (scanl1 (+) (map sqrt [1..]))) + 1

a = sum (filter (> 10) (map (*2) [2..10]))
b = sum $ filter (> 10) $ map (*2) [2..10]
c = sum (filter (> 10) (map (*2) [2..10])) + 1
d = e + 1
 where e = sum $ filter (> 10) $ map (*2) [2..10]