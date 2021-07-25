-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g044 where

import LI11920
import Tarefa0_2019li1g044
import Tarefa1_2019li1g044
import Tarefa2_2019li1g044
import Tarefa3_2019li1g044


-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(1, [[Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Recta Boost 0]], Jogador 0 2 1 0 (Ar 3 35 1)),
     --       (5, [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]], Jogador 1 2 1 0 (Ar 3 35 1)),
       --     (10, [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1],[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0]], Jogador 1 4.7 0 5 (Morto 0.5)),
            (7, [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Boost 0 2],[Recta Terra 0,Rampa Terra 0 2,Recta Lama 2,Rampa Relva 2 1,Recta Relva 1,Recta Relva 1,Recta Boost 1,Rampa Relva 1 3,Recta Relva 3,Recta Terra 3,Recta Terra 3,Recta Boost 3,Rampa Boost 3 1,Rampa Boost 1 0,Recta Boost 0],[Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Rampa Boost 0 1,Recta Lama 1,Rampa Relva 1 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Rampa Lama 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Recta Relva 1]], Jogador 2 7 8 1 (Chao True))
            ]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido. ao bater no chão se a diferença entre a inclinacaoJogador e a in
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j@(Jogador p d v c (Chao h)) = Jogador p d (calcula_velocidade_chao j v (calcula_atrito (idPiso (get_peca (p,round t) m))) t) c (Chao h) 
acelera t m j@(Jogador p d v c e@(Ar alt inc grav)) = Jogador p d (calcula_velocidade_ar j v t) c (Ar alt inc (calcula_gravidade e t))


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j@(Jogador p d v c e)| morto e = movimenta_jog_morto j t 
                              | ar e = movimenta_jog_ar j t (get_peca (p,round t) m)
                              | chao e = movimenta_jog_chao j t (get_peca (p,round t) m) (get_peca (p,round (t+1)) m)
 

r_ar = 0.125
accelGravidade = 1

-- | Calcula o valor do atrito em função do piso da peça em que o jogador está
calcula_atrito :: Piso -> Double
calcula_atrito Terra = 0.25
calcula_atrito Relva = 0.75
calcula_atrito Lama = 1.50
calcula_atrito Boost = -0.5 
calcula_atrito Cola = 3.00

-- | Calcula o valor do parâmetro accelMota 
accelMota :: Jogador -> Double
accelMota (Jogador p d v c (Chao b)) = if (v < 2 && b) then 1 else 0

{-| Dado um jogador, a sua velocidade inicial, o valor de atrito associado à peça em que este se encontra
e o tempo que passou desde a última atualização, a função calcula a velocidade final do jogador
-}
calcula_velocidade_chao :: Jogador -- ^ Jogador 
                   -> Double -- ^ Velocidade inicial
                   -> Double -- ^ Atrito
                   -> Double -- ^ O tempo que passou desde a última atualização
                   -> Double -- ^ Velocidade final 
calcula_velocidade_chao j v a t = if (v' > 0)  then v' else 0
                                      where v' = v + ((accelMota j) - a * v) * t
{-| Dado um jogador, a sua velocidade inicial e o tempo que passou desde a última 
atualização, a função calcula a velocidade final do jogador
-}
calcula_velocidade_ar :: Jogador -- ^ Jogador
                      -> Double -- ^ Velocidade inicial
                      -> Double -- ^ O tempo que passou desde a última atualização 
                      -> Double -- ^ Velocidade final
calcula_velocidade_ar j v t = if (v' > 0)  then v' else 0
                                where v' = v - (r_ar * v * t) 

{-|Calcula um novo valor para a gravidade do jogador em função da velocidade causada pela gravidade
atual do jogador e do instante de tempo em questão
-}
calcula_gravidade :: EstadoJogador -- ^ Estado do jogador
                  -> Double -- ^ O tempo que passou desde a última atualização
                  -> Double -- ^ Nova gravidade
calcula_gravidade (Ar _ _ g) t= g + accelGravidade * t

-- | Dado um jogador e uma velocidade, a função atualiza o parâmetro velocidade do jogador
atualiza_velocidade :: Jogador -> Double -> Jogador
atualiza_velocidade (Jogador p d v c ej) v' = (Jogador p d v' c ej)

-- | Realiza a tarefa de movimentar um jogador quando está morto no chão 
movimenta_jog_morto :: Jogador -- ^ Jogador
                    -> Double -- ^ O tempo que passou desde a última atualização
                    -> Jogador -- ^ Jogar com o timeout atualizado
movimenta_jog_morto (Jogador p d v c (Morto t)) t'
            | t > 0 && t > t' = Jogador p d v c (Morto (t-t'))
            | otherwise = Jogador p d 0 c (Chao False)

-- | Realiza a tarefa de movimentar um jogador quando está no ar 
movimenta_jog_ar :: Jogador -- ^ Jogador
                 -> Double -- ^ O tempo que passou desde a última atualização
                 -> Peca -- ^ Peca onde o jogador se encontra
                 -> Jogador -- ^ Jogar com o timeout atualizado
movimenta_jog_ar j@(Jogador p d v c (Ar h i g)) t peca 
        | dif_inclinacao_jog j peca >= 45 = Jogador p d 0 c (Morto 1) 
        | otherwise = Jogador p (h+t*(v*cos((i*pi)/180)-1)) v c (Ar (h+t*(v*sin((i*pi)/180)-g)) i g) 

-- | Calcula a diferença entre o parâmetro inclinacaoJogador e a inclinação da peça onde o jogador se encontra
dif_inclinacao_jog :: Jogador -> Peca -> Double
dif_inclinacao_jog  (Jogador _ _ _ _ (Ar _ i _)) p = abs (i- calcula_inclinacao p) 

-- | Realiza a tarefa de movimentar um jogador quando está no chao 
movimenta_jog_chao :: Jogador -- ^ Jogador
                   -> Double -- ^ O tempo que passou desde a última atualização
                   -> Peca -- ^ Peca onde o jogador se encontra
                   -> Peca -- ^ Peca seguinte
                   -> Jogador -- ^ Jogar com o timeout atualizado
movimenta_jog_chao j@(Jogador p d v c (Chao b)) t p1 p2 
        | calcula_inclinacao p2 > calcula_inclinacao p1 = Jogador p (d+1) v c (Chao b)
        | otherwise = (Jogador p (d+1) 0 c (Ar h i 0))
            where i = calcula_inclinacao p2
                  h = calcula_altura (d+1) p2

-- | Calcula a diferença entre a peça em que o jogador se encontra e a peça seguinte
dif_inclinacao :: Peca -> Peca -> Double
dif_inclinacao pi pf = abs $ calcula_inclinacao pi - calcula_inclinacao pf



verifica_limite :: Jogador -- ^ Jogador
                -> Double -- ^ Inclinação inicial
                -> Double -- ^ Inclinação final
                -> Double -- ^ Altura final da peça atual
                -> Jogador -- ^ Jogador atualizado
verifica_limite (Jogador p d v c (Chao h)) ii ifinal al | ifinal >= ii = (Jogador p d v c (Chao h))
                                                        | otherwise = (Jogador p d v c (Ar al ii 0))
verifica_limite (Jogador p d v c (Ar _ ij _)) ii ifinal _ | (ij - ii) >= 45 = (Jogador p d 0 c (Morto 1))
                                                          | otherwise = (Jogador p d v c (Chao False))
