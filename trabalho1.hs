--Grupo:
--Pedro Henrique Goncalves Teixeira - 11821BCC008
--Marillia Soares Rodrigues - 11821BCC020

--TIPOS E VALORES--
type Jogador = (Int, (Int, Int), String, [(String, Int)] )

data Drop = Patins | Bomba | Arremesso deriving (Eq, Show)
data Objeto = Buraco | Grama | Pedra | Parede | Presente Drop | Jogador Int deriving (Show,Eq)

type Item = Objeto
type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]

--CELULAS--

celulaPedra :: Celula
celulaPedra = [Pedra]

celulaGrama :: Celula
celulaGrama = [Grama]

celulaPG :: Celula
celulaPG = [Parede, Grama]

celulaPlayer :: Celula
celulaPlayer = [Grama, Jogador 1]

celulaBomba :: Celula
celulaBomba = [Presente Bomba, Grama]

celulaPatins :: Celula
celulaPatins = [Presente Patins, Grama]

celulaArremesso :: Celula
celulaArremesso  = [Presente Arremesso , Grama]

celulaPPB :: Celula
celulaPPB = [Parede, Presente Bomba, Grama]

celulaPPP :: Celula
celulaPPP = [Parede, Presente Patins, Grama]

celulaPPA :: Celula
celulaPPA = [Parede, Presente Arremesso, Grama]

--LINHAS-

l1 :: Linha
l1 = [celulaPedra, celulaGrama, celulaPedra, celulaPedra, celulaGrama, celulaBomba, celulaPedra, celulaBomba]

l2 :: Linha
l2 = [celulaGrama, celulaPedra, celulaBomba, celulaGrama, celulaGrama, celulaBomba, celulaBomba, celulaBomba]

l3 :: Linha
l3 = [celulaGrama, celulaPedra, celulaGrama, celulaPedra, celulaBomba, celulaBomba, celulaPedra, celulaBomba]

l4 :: Linha
l4 = [celulaGrama, celulaPlayer, celulaGrama, celulaGrama, celulaPlayer, celulaGrama, celulaPlayer, celulaGrama]

l5 :: Linha
l5 = [celulaGrama, celulaPedra, celulaPlayer, celulaGrama, celulaPlayer, celulaPedra, celulaGrama, celulaPlayer]

l6 :: Linha
l6 = [celulaGrama, celulaPedra, celulaPedra, celulaPlayer, celulaGrama, celulaPedra, celulaPedra, celulaPlayer]

l7 :: Linha
l7 = [celulaPedra, celulaGrama, celulaPedra, celulaPlayer, celulaPlayer, celulaPedra, celulaPlayer, celulaGrama]

l8 :: Linha
l8 = [celulaGrama, celulaPedra, celulaPlayer, celulaPlayer, celulaPedra, celulaGrama, celulaPlayer, celulaPedra]

--TABULEIRO--

tabuleiro :: Tabuleiro
tabuleiro = [l1, l2, l3, l4, l5, l6, l7, l8]
