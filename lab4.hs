--QUESTAO 01
fatorial :: Int -> Int
fatorial x
    | x == 1 = 1
    | otherwise = x * fatorial (x-1)

--QUESTAO 02
fibo :: Int -> Int
fibo x
    | x == 1 = 1
    | x == 2 = 1
    | otherwise = fibo(x-2) + fibo(x-1)

{---QUESTAO 03
n_tri :: Int -> Int
n_tri x
    | x == 1 = 1
    | x == 0 = 0
    | otherwise = n_tri(x) + n_tri(x-1)
-}

--QUESTAO 04
fibo2 :: Int -> [(Int)]
fibo2 y = [x | x <-y, fibo1 x]