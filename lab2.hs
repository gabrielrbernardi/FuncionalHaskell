--QUESTAO 2
type Val = (Float, Float, Float)
conversao :: Float -> Val
conversao x = (x, x*3.96, x*4.45)

--QUESTAO 3
bissexto :: Int -> Bool
bissexto x | (mod x 400 == 0) = True
    | (mod x 4 == 0) && (mod x 100 /= 0) = True
    | otherwise = False

--QUESTAO 4
type Data = (Int, Int, Int)
bissextos :: Data -> Bool
bissextos (d, m, a) = bissexto a

--QUESTAO 5
valida :: Data -> Bool
valida (d, m, a) 
    | (m == 2) && (d >=29) = False                                                  -- Restricao em relacao ao mes de fevereiro
    | (m == 4 || m == 6 || m == 9 || m == 11) && (d > 30) = False                   -- Restricao em relacao a data bruta
    | (d > 0 && d <= 31) && (m > 0 && m <= 12) = True                               -- Restricao em relacao aos meses
    | otherwise = False

--QUESTAO 6
precede :: Data -> Data -> String
precede (d1, m1, a1) (d2, m2, a2)
    | d1 < d2 || m1 < m2 || a1 < a2 = show "Data 1 Anterior"
    | d1 == d2 && m1 == m2 && a1 == a2 = show "Data 1 igual a Data 2"
    | otherwise = show "Data 1 Posterior"

--QUESTAO 7
type Livro = (String, String, String, String, Int)                                  -- código do livro, título do livro, autor, editora e ano de publicação.
type Aluno = (String, String, String, String)                                       -- código do aluno, nome, e-mail e telefone.
type Emprestimo = (String, String, Data, Data, String)                              -- código do livro, código do aluno, data de empréstimo, data de devolução e situação
-- type Livros = [Livro]
-- type Alunos = [Aluno]
-- type Emprestimos = [Emprestimo]
--QUESTAO 8
biblioteca :: Emprestimo -> Data -> String
biblioteca (e1, e2, (de, me, ae), (dd, md, ad), e5) (d, m, a)
    | e5 == "aberto" = show "Emprestimo em aberto"
    | otherwise = show "Emprestimo OK"
