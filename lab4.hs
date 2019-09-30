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

--QUESTAO 04 -- de acordo com a professora, nao precisa fazer
-- fibo2 :: Int -> [(Int)]
-- fibo2 y = [x | x <-y, fibo1 x]

--QUESTAO 05
fibo2 :: Int -> Int
fibo2 1 = 1
fibo2 2 = 1
fibo2 n = fibo2(n-2)+fibo2(n-1)

retornaFibo :: Int->[Int]
retornaFibo x = [fibo x | x <- [1..50]]

retornaFibo2 :: Int->[Int]
retornaFibo2 x = [fibo2 x | x <- [1..50]]

--R: acredito que o algoritmo que calcula fibonacci com casamento de padroes seja mais eficiente

--QUESTAO 06
potencia2 :: Int -> Int
potencia2 x
    | x == 0 = 1
    | otherwise = 2^potencia2(x-1)

--QUESTAO 07

--QUESTAO 08

--QUESTAO 09
--usando guardas
mdcg :: (Int,Int) -> Int
mdcg (m,n)
    | n == 0 = m
    | otherwise = mdcg (n, (mod m n))
--usando casamento de padroes
mdc :: (Int,Int) -> Int
mdc (m,0) = m
mdc (m,n) = mdc (n, (mod m n))

--QUESTAO 10
--usando guardas
binomialg :: (Int,Int) -> Int
binomialg (n,0)  = 1
binomialg (n,k)
| k == 0 = 1
| k == n = 1
| otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

--usando casamento de padroes
binomial :: (Int,Int) -> Int
binomial (n,0)  = 1
binomial (n,k) = if (k == n) then 1
                             else binomial (n-1,k) + binomial (n-1,k-1)