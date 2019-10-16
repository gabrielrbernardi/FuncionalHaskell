-- *************** 
-- Uso do case

seleciona_pares [] = []
seleciona_pares (x:xs) = if (mod x 2==0) then x: seleciona_pares xs else seleciona_pares xs

firstPar list = case (seleciona_pares list) of
       [] -> "Lista Vazia"
       (x:_) -> show x



-- ***************
-- Uso do Where

-- 1a Versao analisaIMC

analisaIMC imc
 | imc<=18.5 = "Voce esta abaixo do peso ideal."
 | imc<=25.0 = "Seu peso parece normal. "
 | imc<=30.0 = "Voce esta acima do peso ideal."
 | otherwise = "Voce esta obeso."



-- 2a Versao analisaIMC

analisaIMC1 peso altura
 | peso/(altura)^2<=18.5 = "Voce esta abaixo do peso ideal."
 | peso/(altura)^2<=25.0 = "Seu peso parece normal. "
 | peso/(altura)^2<=30.0 = "Voce esta acima do peso ideal."
 | otherwise = "Voce esta obeso."


-- 3a Versao analisaIMC (uso do where)

analisaIMC2 peso altura
 | imc<=18.5 = "Voce esta abaixo do peso ideal."
 | imc<=25.0 = "Seu peso parece normal. "
 | imc<=30.0 = "Voce esta acima do peso ideal."
 | otherwise = "Voce esta obeso."
 where
     imc = peso/(altura)^2


-- 4a Versao analisaIMC (uso do where)

analisaIMC3 peso altura
 | imc<=normal1 = "Voce esta abaixo do peso ideal."
 | imc<=normal2 = "Seu peso parece normal. "
 | imc<=obeso = "Voce esta acima do peso ideal."
 | otherwise = "Voce esta obeso."
 where
     imc = peso/(altura)^2; 
     normal1=18.5; 
     normal2=25;
     obeso=30.0

       

-- Fibonacci 3a versao

fiblist = 1:1:[ a+b | (a,b) <-zip fiblist (tail fiblist) ]
fibo3 n = fiblist !! n



{- fiblist = 1:1:[ a+b | (a,b) <-zip fiblist (tail fiblist) ]

1a) fib = [1,1...]  tail fib = [1,...]  -> zip = [(1,1),...] (a,b)=(1,1) -> a+b = 2
fiblist=1:1:2:...

2a) fib = [1,1,2...]  tail fib = [1,2...]  -> zip = [(1,1),(1,2)...] (a,b)=(1,2) -> a+b = 3
fiblist=1:1:2:3...

3a) fib = [1,1,2,3...]  tail fib = [1,2,3...]  -> zip = [(1,1),(1,2)(2,3)...] (a,b)=(2,3) -> a+b = 5
fiblist=1:1:2:3:5...

4a) fib = [1,1,2,3,5...]  tail fib = [1,2,3,5...]  -> zip = [(1,1),(1,2)(2,3),(3,5)...] (a,b)=(3,5) -> a+b = 8
fiblist=1:1:2:3:5:8...
-}


-- Fibonacci 4a versao (uso do where)

fibo4 n = fiblist2 !! n
 where fiblist2 = 1:1:[ a+b | (a,b) <-zip fiblist2 (tail fiblist2) ]



-- Funcao raizes

raizes a b c = ((-b+sqrt(d))/(2*a), (-b-sqrt(d))/(2*a), d)
        where d=b^2 - 4*a*c


fu a b = 2*a + 3*b

-- ***************
-- Uso do let

--fui :: Int -> Int -> Int
-- fui a b = let  {y=a*b;g x = (x+y)*y} in               g (2*a) + g (3 ^b)



- Funcao raizes 2a vers�o

raizes2 a b c =  let {d = (b^2 - 4*a*c)} in ((-b+sqrt(d))/(2*a), (-b-sqrt(d))/(2*a), d)

-- Fibonacci 5a versao  

fibo5 n = let fiblist2 = 1:1:[ a+b | (a,b) <-zip fiblist2 (tail fiblist2) ] in fiblist2 !! n
