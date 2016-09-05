circleArea :: Float -> Float
circleArea r = pi * r ** 2

listSum :: [Float] -> Float
listSum [] = 0
listSum (x:xs) = x + listSum xs

totalArea r1 r2 r3 = listSum[circleArea r1, circleArea r2, circleArea r3]