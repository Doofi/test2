{-
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)


surface :: Shape -> Float
surface (Circle _ rad) = pi * rad ** 2
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1

)
-}

data Number = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

days :: Day -> Int -> [Day]
days day num = take num rep
    where rep = concat . repeat $ [day ..] ++ [Monday .. (pred day)]

