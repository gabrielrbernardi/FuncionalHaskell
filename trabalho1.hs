--QUESTAO 01
triangulo :: Int -> Int -> Int -> String
triangulo x y z
    | (x >= y+z) || (y >= x+z) || (z >= x+y) = "Nao eh triangulo"
    | (x == y) && (y == z) && (z == x) = "Equilatero"
    | (x == y) || (y == z) || (z == x) = "Isosceles"
    | (x /= y) && (x /= z) && (y /= z) = "Escaleno"

--QUESTAO 02
type Raiz = (Float, Float)
equacao :: Float -> Float -> Float -> Raiz
equacao a b c 
    | (delta a b c > 0) = ((abs (raiz1 (-b) (delta a b c) a)), (raiz2 (-b) (delta a b c) a)*(-1))
    | (delta a b c == 0) = ((abs (raiz1 (-b) (delta a b c) a)), (raiz2 (-b) (delta a b c) a)*(-1)) 

raiz2 :: Float -> Float -> Float -> Float
raiz2 b d a = (((-b) + (sqrt d)) / (2*a))

raiz1 :: Float -> Float -> Float -> Float
raiz1 b d a = (((-b) - (sqrt d)) / (2*a))

    -- | otherwise
delta :: Float-> Float-> Float-> Float
delta x y z = ((y**2)-(4*x*z))

--QUESTAO 03
passagem :: Float -> Int -> Float
passagem valTot idade
    | idade >= 60 = valTot*0.6
    | idade < 2 = valTot*0.1
    | idade <= 10 = valTot*0.5
    | otherwise = valTot

--QUESTAO 04
-- A resolucao dessa questao estara em um arquivo anexado e que sera enviado junto

--QUESTAO 05
qtdNeg :: [Int] -> [Int]
qtdNeg y = [x | x <- y, x < 0]

--QUESTAO 06
distancias :: [(Float,Float)] -> [Float]
distancias [(a,b)] = [x | x <- [sqrt(a**2 + b**2)]]

--QUESTAO 07
ehPrimo :: Int -> Bool
ehPrimo x = divisores x == [1,x]

divisores :: Int -> [Int]
divisores 1 = [1]
divisores x = 1:[ y | y <- [2..(div x 2)], mod x y == 0] ++ [x]

primos :: Int -> Int -> [Int]
primos x y = [x | x <- [x..y], ehPrimo x]

--QUESTAO 08
-- notasTroco :: Int -> [[Int]]
notasTroco y = [x | x <- [1..45]]

--QUESTAO 09
mdc :: Int -> Int -> Int
mdc a b
    | a < b = mdc b a
    | b == 0 = a
    |otherwise = mdc b (mod a b)

mmcBasico :: Int -> Int -> Int
mmcBasico x y = div (x * y) (mdc x y)

mmc :: Int -> Int -> Int -> Int
mmc x y z = mmcBasico x (mmcBasico y z)

--QUESTAO 11
fizzbuzz n = [if mod x 15 == 0 then "FizzBuzz" 
              else if mod x 3 == 0 then "Fizz" 
              else if mod x 5 == 0 then "Buzz"
              else show x | x <- [1..n]]

--QUESTAO 12
conta_ocorrencias :: Int -> [Int] -> Int
conta_ocorrencias e [] = 0
conta_ocorrencias e (x:y) | x == e = 1 + conta_ocorrencias e y
                          | otherwise = conta_ocorrencias e y

--QUESTAO 13
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n list = if (conta_ocorrencias n list == 1)
                         then True
                         else False

--QUESTAO 14
intercala::[Int]->[Int]->[Int]
intercala x [] = x
intercala [] x = x
intercala (a:x1) (b:y1) = a: b: intercala x1 y1

--QUESTAO 15
type Contato = ([Char],[Char],[Char],[Char])
type Agenda = [Contato]

listaContato :: Agenda
listaContato = [("Jim","RJ","1234-5678","jim@dundermifflin.com"),
                ("Michael","AM","1214-4546","jichael@dundermifflin.com"),
                ("Dwight","DF","2448-7296","dwight@dundermifflin.com"),
                ("Pamela","MG","1224-2412","pamela@dundermifflin.com"),
                ("Andrew","SC","1235-7111","andrew@dundermifflin.com")]

procuraNumero::[Char]->Agenda->[Char]
procuraNumero x [] = "Telefone Desconhecido"
procuraNumero x ((a,_,t,_):cs) | x == t = a                             
                               | otherwise = procuraNumero x cs

--QUESTAO 17
{-insere ::(Ord a) => [a] -> [a]
insere [] = []
insere (x:b) = insere_ord1 x (insere b)

insere_ord ::(Ord a) => Int -> [a] -> [a]
insere_ord num [] = []
insere_ord num (x:b) = num ++ [a] insere_ord1 x (insere_ord b)

insere_ord1 :: (Ord a) => a -> [a] -> [a]
insere_ord1 x [] = [x]
insere_ord1 x (y:a)
    | x <= y = (x:y:a)
    | otherwise = y: (insere_ord1 x a)
-}

--QUESTAO 18
reverte a = reverse a

--QUESTAO 19
sem_repetidos :: (Ord t) => [t] -> [t]
sem_repetidos [] = []
sem_repetidos [a]= [a]
sem_repetidos (a:b:x)
    | a == b = sem_repetidos (b:x)
    | otherwise= a : sem_repetidos (b:x)

--QUESTAO 20
-- A resolucao dessa questao estara em um arquivo anexado e que sera enviado junto

--QUESTAO 21
conta_maior5 :: [Int] -> [Int]
conta_maior5 [] = []
conta_maior5 (a:t) = if a > 5 then a : conta_maior5 t
                              else conta_maior5 t
            
--QUESTAO 22