--QUESTAO 03
listQuad = [x^2 | x <- [1..100]]

--QUESTAO 04
listQuadPar = [x^2 | x <- [1..100], even x]

--QUESTAO 05
quadrados x y = [x^2 | x <- [x..y]]

--QUESTAO 06
selecionaImpares :: [Int] -> [Int]
selecionaImpares y = [x | x <-y, odd x]

--QUESTAO 07
tabuada :: Int -> [Int]
tabuada y = [y*x | x <- [1..10]]

--QUESTAO 08
bissextos :: [Int] -> [Int]
bissexto :: Int -> Bool
bissexto x | (mod x 400 == 0) = True
    | (mod x 4 == 0) && (mod x 100 /= 0) = True
    | otherwise = False

bissextos y = [x | x <- y, bissexto x]


--QUESTAO 09

sublistas :: [[Int]] -> [Int]
sublistas y = [x | a <- y, x <- a]


--QUESTAO 10
type Data = (Int, Int, Int)

type Livro = (String, String, String, String, Int)                                  -- código do livro, título do livro, autor, editora e ano de publicação.
type Aluno = (String, String, String, String)                                       -- código do aluno, nome, e-mail e telefone.
type Emprestimo = (String, String, Data, Data, String)                              -- código do livro, código do aluno, data de empréstimo, data de devolução e situação

biblioteca :: Emprestimo -> Data -> String
biblioteca (e1, e2, (de, me, ae), (dd, md, ad), e5) (d, m, a)
    | e5 == "aberto" = show "Emprestimo em aberto"
    | otherwise = show "Emprestimo OK"