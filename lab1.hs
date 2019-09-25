--QUESTAO 2
dobro :: Int -> Int
dobro d = d * 2
--QUESTAO 3
dobro2 :: Int -> Int
dobro2 d = dobro d * 2
--QUESTAO 4
hip :: Float -> Float -> Float
hip l1 l2 = sqrt((l1**2) + (l2**2))
--QUESTAO 5
dist_cart :: Float -> Float -> Float -> Float -> Float
dist_cart xa ya xb yb = sqrt(((xb-xa)**2) + ((yb-ya)**2))
dis
--QUESTAO 6a
par :: Int -> Bool
par a = if mod a 2 == 0 
    then True
    else False
--QUESTAO 6b
imp :: Int -> Bool
imp a = if par a == True
    then False
    else True
--QUESTAO 7
conv_f_c :: Float -> Float
conv_f_c a = ((a-32)*5)/9
--QUESTAO 8
maior :: Int -> Int -> Int
maior a b = if a > b 
   then a
   else b
--QUESTAO 9
maior3 :: Int -> Int -> Int -> Int
maior3 a b c 
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c
--QUESTAO 10
verificaValor :: Int -> Int
verificaValor x
    | x > 0 = 1
    | x == 0 = 0
    | otherwise = -1
--QUESTAO 11
ehChar::Char->Bool
ehChar c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
ehChar c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
--QUESTAO 12
ladoTriangulo :: Float -> Float -> Float -> Bool
ladoTriangulo a b c
    | (abs(b-c) < a && a < (b + c)) && (abs(a-c) < b && b < (a + c)) && (abs(a-b) < c && c < (a + b)) = True
    | otherwise = False
