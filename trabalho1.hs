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
divisores x = 1:[ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]

primos :: Int -> Int -> [Int]
primos x y = [x | x <- [x..y], ehPrimo x]

--QUESTAO 08
-- notasTroco :: Int -> [[Int]]
notasTroco y = [x | x <- [1..45]]

--QUESTAO 09
mmc :: (Int, Int, Int) -> Int
mmc (a, b, c)
    | 