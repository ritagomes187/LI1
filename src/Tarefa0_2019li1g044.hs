-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g044 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- Converte um angulo em graus para radianos
rad :: Angulo -> Double
rad a = a * (pi/180)


-- Converte um vetor para a forma cartesiana
cart :: Vetor -> Vetor
cart (Cartesiano x y) = Cartesiano x y 
cart (Polar d a) = Cartesiano (d*(cos $ rad a)) (d*(sin $ rad a))

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores v1 v2 = Cartesiano (x1+x2) (y1+y2)
        where Cartesiano x1 y1 = cart v1
              Cartesiano x2 y2 = cart v2

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores v1 v2 = Cartesiano (x1-x2) (y1-y2)
    where Cartesiano x1 y1 = cart v1
          Cartesiano x2 y2 = cart v2

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor k v = Cartesiano (k*x) (k*y)
    where Cartesiano x y = cart v

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto, Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam (p1, p2) (p3, p4) = ta>=0 && ta<=1 && tb>=0 && tb<=1
    where (Cartesiano x1 y1) = cart p1
          (Cartesiano x2 y2) = cart p2
          (Cartesiano x3 y3) = cart p3
          (Cartesiano x4 y4) = cart p4
          ta = ((y3-y4)*(x1-x3) + (x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3)) 
          tb = ((y1-y2)*(x1-x3) + (x2-x1)*(y1-y3)) / ((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1, p2) (p3, p4) | intersetam (p1, p2) (p3, p4) = somaVetores p1 (multiplicaVetor ta (subtraiVetores p2 p1))
                             | otherwise = error"As retas nao se intersetam"
            where (Cartesiano x1 y1) = cart p1
                  (Cartesiano x2 y2) = cart p2
                  (Cartesiano x3 y3) = cart p3
                  (Cartesiano x4 y4) = cart p4
                  ta = ((y3-y4)*(x1-x3) + (x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3)) 



-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i>=0 && i<length l

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int, Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int, Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0, 0)
dimensaoMatriz ([]:xs)=(0,0)
dimensaoMatriz l = (length l, length(head l))

-- Verifica se a dimensao de uma matriz e valida (se todas as linhas tem o mesmo numero de elementos)
dimensaoValida :: Matriz a -> Bool
dimensaoValida [] = True
dimensaoValida (x:xs) = auxMat (length x) xs

-- Funcao auxiliar => dado o n de elementos da 1a linhas, esta se todas as linhas tem o mesmo n de elementos
auxMat :: Int -> Matriz a -> Bool
auxMat _ [] = True
auxMat n (x:xs) | n == (length x) = auxMat n xs
                | otherwise = False 

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida (x,y) l | (length l >0) && (x<length l) && (y< length(head l)) =True
                             | otherwise= False

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a | a>=0 && a<360 = a 
                  | a>360 = normalizaAngulo (a-360)
                  | otherwise = normalizaAngulo (a+360)

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (x:xs) = x
encontraIndiceLista i (x:xs) | eIndiceListaValido i (x:xs) = encontraIndiceLista (i-1) xs
                             | otherwise = error "O indice nao e valido"
-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i y (x:xs) | eIndiceListaValido i (x:xs) = atualizaAux i y (x:xs)
                               | otherwise = (x:xs)


-- Funcao auxiliar que, quando o indice e valido, substuiu um dado elemento num indice 
atualizaAux :: Int -> a -> [a] -> [a]
atualizaAux _ _ [] = []
atualizaAux i y (x:xs) | i == 0 = (y:xs)
                       | otherwise = x:(atualizaAux (i-1) y xs)

                                 
-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (l, c) m | ePosicaoMatrizValida (l, c) m = encontraIndiceLista c (encontraIndiceLista l m)
                               | otherwise = error"A posicao nao e valida"

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (l, c) x m | ePosicaoMatrizValida (l, c) m = atualizaPosicaoAux (l, c) x m
                                 | otherwise = m

-- Percorre a matriz ate a posicao e substitui o elemento
atualizaPosicaoAux :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoAux _ _ [] = []
atualizaPosicaoAux (0, c) y (x:xs) = (atualizaIndiceLista c y x):xs
atualizaPosicaoAux (l, c) y (x:xs) = x:(atualizaPosicaoAux (l-1, c) y xs)

