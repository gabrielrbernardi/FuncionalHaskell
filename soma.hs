soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma(xs)
