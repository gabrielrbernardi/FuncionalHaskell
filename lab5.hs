--QUESTAO 01
npares :: [Int] -> Int
npares[] = 0
npares(x:xs)
    | ev == True = 1 + (npares xs )
    | otherwise = npares xs
    where ev = even x

--QUESTAO 02
dobraPos :: [Int] -> [Int]
dobraPos [] = []
dobraPos (x:xs) = x*2 : dobraPos xs

--QUESTAO 03
produtorio::[Int]->Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

--QUESTAO 05
fatores n = [i | i<-[1..n], mod n i == 0]

--QUESTAO 06
{-comprime :: [[a]] -> [(Int,a)]
comprime  [[]] = []
comprime  [(x:xs)] = (1 + length (takeWhile x xs), x) : comprime [(dropWhile x xs)]-}

--QUESTAO 07
tamanho :: (Num b) => [a] -> b
tamanho [] = 0
tamanho (_:x) = 1 + tamanho x

--QUESTAO 08
pertence :: Eq a => ([a],a) -> Bool
pertence ([], _) = False
pertence ((x:xs), y) = if x == y then True else pertence (xs, y)

--QUESTAO 10
maior :: [Int] -> Int
maior x = max x

--QUESTAO 11
