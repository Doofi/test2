module Eulerprobs where

import qualified Data.List as List

-- F U N C T I O N S --


fib = 1 : 1 : [(a + b) | (a, b) <- zip fib (tail fib)]


nextprime :: Integer -> Integer -> Integer
nextprime x y
    | mod y x == 0 = x
    | x /= 2 = nextprime (x + 2) y
    | otherwise = nextprime (x + 1) y


primefactors' :: Integer -> [Integer] -> [Integer]
primefactors' curval primes



    | (curval == 1) = init primes
    | otherwise = primefactors' (quot curval factor) (factor : primes)
        where
            factor = (nextprime (primes !! 0) curval)


primefactors var =
    primefactors' var [2]


isPalindrome :: String -> Bool
isPalindrome str =
    reverse str == str


isqrt :: Integer -> Integer
isqrt x = floor . sqrt . fromIntegral $ x

primes = 2 : 3 : filter myfilter [5, 7..]
    where myfilter x =
            all (/= 0) $ map (mod x) (takeWhile (<= isqrt x) primes)



stringtolist :: [Char] -> [Int]
stringtolist str = map (\c -> read (c : []) :: Int) str




-- P R O B L E M S --


-- 1 --

sol1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- 2 --


sol2 = sum [x | x <- lst, mod x 2 == 0] where
    lst = takeWhile (< 4000000) fib

-- 3 --

var3 = 600851475143
sol3 = primefactors var3

-- 4 --

sol4 = maximum
    [a * b | a <- [100..999], b <- [a..999],
    isPalindrome . show . (*) a $ b]


-- 5 --

sol5 = [x | x <- [1..],
    mod x 11 == 0, mod x 12 == 0, mod x 13 == 0, mod x 14 == 0, mod x 15 == 0,
    mod x 16 == 0, mod x 17 == 0, mod x 18 == 0, mod x 19 == 0, mod x 20 == 0]

-- 6 --


sol6 = var6_2 - var6_1
    where 
        var6_1 = sum [x ** 2 | x <- [1..100]]
        var6_2 = (^2) $ sum [1..100]


-- 7 --

sol7 = primes !! 10000


-- 8 --

getlargestproduct :: String -> Int
getlargestproduct longstr =
    let
        intlist = stringtolist longstr
    in
        0


-- 9 --


sol9 :: [Integer]
sol9 = [a * b * (1000-a-b) | a <- [1..1000], b <- [a..1000],
        a*a + b*b == (1000 -a -b)*(1000 -a -b)]

-- 10 --

sol10 = sum $ takeWhile (< 2000000) primes