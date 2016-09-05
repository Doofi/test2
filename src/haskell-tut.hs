import Data.List
import System.IO

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

{-
AND : &&
OR : ||
not : not(True)

LISTS:

list1 = [1, 2, 3]
list2 = 4 : 5 : 6 : []
list3 = list1 ++ list2
list3
> [1, 2, 3, 4, 5, 6]

multList = [[3, 5, 7], [11, 13, 17]]

add0tolist = 0 : list1

revlist = reverse list1 > [3, 2, 1, 0]
isListEmpty = null list1 > False

secondElement = list1 !! 2 > 2
firstElement = head list1
lastElement = last list1
InitElements = init list1 > [3, 2, 1]

first3Elements = take 3 list1

removeElements = drop 3 list1

is6inList = 6 `elem` list1 > False

maxinList = maximum list1
mininList = minimum list1


newlist = [2, 3, 5]
prodprimres = product newlist > 30

zerototen = [0..10]

evenList = [2,4..20]

letterList = ['A', 'C'..'Z']

infinPow10 = [10, 20..]

many2s = take 10 (repeat 2)


cycleList = take 10 (cycle [1, 2, 3, 4, 5])
> [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]

listTimes2 = [x * 2 | x <- [1..10]]
> [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

listTimes3andfilter = [x * 3 | x <- [1..10], x * 3 <= 50]
> [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]


List Comprehension:
Prelude> xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
Prelude> [a+b | (a,b) <- xs]  
>> [4,7,6,8,11,4]


-- show True >> "True"
-- read "True" >> True

-}

divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, mod x 9 == 0]

sortedList = sort [8, 3, 5]

sumOfLists = zipWith (+) [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]

listBiggerThan5 = filter (>5) sumOfLists

evensUpTo20 = takeWhile (<= 20) [2, 4..]

-- foldl : executes an operation on each element of a list, starting left
-- foldr : executes an operation on each element of a list, starting right

pow3List = [3 ** n | n <- [1..10]]

multTable = [x * y | y <- [1..10]] | x <- [1..10]

bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith

bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]

addresses = ["123 Main", "234 North", "567 South"]

namesNAddresses = zip names addresses



addMe :: Int -> Int -> Int

-- funcName param1 param2 = operations (returned value)

addMe x y = x + y
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You can adult"
whatAge _ = "Nothing Important"

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3 * 2 = 6 
-- 2 * 1 = 2
-- 1 * factorial(0) : 1
    
prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | (n `mod`2) == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String

whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age >= 6) && (age <= 10) = "Elementary School"
    | (age >= 10) && (age <= 14) = "Middle School"
    | (age >= 14) && (age <= 8) = "High School"
    | otherwise = "Go to college!"


batAvgRating :: Double -> Double -> String

batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "You're doing pretty good!"
    | otherwise = "You're a Superstar!"
    where avg = hits / atBats

getListItems :: [Int] -> String

getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "your list contains " ++ show x ++ "and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ "and the rest are " ++
    show xs

getFirstItem :: String -> String

getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1, 2, 3, 4, 5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int

doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)

getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4

dbl1To10 = map (\x -> x * 2) [1..10]

doubleEvenNumber y =
    if (mod y 2 /= 0)
        then y
        else y * 2

getClass :: Int -> String

getClass n = case n of
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary School"
    _ -> "Go away"

data Customer = Customer String String Double
    deriving Show

tomSmith :: Customer
tomSmith = Customer "tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double

getBalance (Customer _  _ b) = b

data RPS = Rock | Paper | Scissors

shoot :: RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Paper Beats Rock"
shoot Scissors Paper = "Paper Beats Rock"
shoot Scissors Rock = "Paper Beats Rock"
shoot Paper Scissors = "Paper Beats Rock"
shoot Rock Paper = "Paper Beats Rock"
shoot _ _ = "Error"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving show

area :: Shape -> Float#

area (Circle _ _ r) = pi * r ** 2
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y))











