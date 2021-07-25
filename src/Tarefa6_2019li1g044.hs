-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g044 where

import LI11920
import Data.List -- funcao !!

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i e@(Estado m lj) | decide_disparo i e = Just Dispara 
                      | decide_acelera i e = Just Acelera
                      | otherwise = Nothing

-- | Dado o identificador de um jogador, a funcao devolve o proprio jogador
-- assume que o identificador e valido ie i < length l 
-- o idenficiador comeca em 0 ou 1 ????
jog_i :: Int -- ^ Identificador do jogador
      -> [Jogador] -- ^ Lista de jogs
      -> Jogador -- ^ Jogador
jog_i i jogs = jogs !! i 

-- | Conta o numero de jogadores numa dada pista
conta_jog_pista :: Int -- ^ Numero da pista
                -> [Jogador] -- ^ Lista dos jogadores
                -> Int -- ^ Numero de jogadores numa pista
conta_jog_pista _ [] = 0
conta_jog_pista i ((Jogador p _ _ _ _):js) | i == p = 1 + conta_jog_pista i js
                                           | otherwise = conta_jog_pista i js 

-- | Dado um jogador e uma lista de jogadores, testa se existem (quantos) jogadores atras dele nessa pista 
jog_atras :: Jogador -- ^ Jogador j
          -> [Jogador] -- ^ Lista dos jogadores
          -> Int -- ^ Numero de jogadores na pista do jogador j que estao atras dele
jog_atras _ [] = 0
jog_atras j@(Jogador pbot dbot _ _ _) ((Jogador p d _ _ _):js)
          | pbot==p && d<dbot = 1 + jog_atras j js
          | otherwise = jog_atras j js

-- | Determina se o bot deve ou nao disparar cola 
decide_disparo :: Int -- ^ Identifcador do jogador associaodo ao bot
               -> Estado -- ^ estado para o qual o bot deve tomar a decisao
               -> Bool -- ^ booleano associado à decisao 
decide_disparo i (Estado m jogs) = (jog_atras (jog_i i jogs) jogs) > 0  



--um jogador esta perto de outro relativamente a um eixo vertical se estiver numa de 5 peças contiguas (abs(dbot-d)<=1)
--do mapa e na linha imediatamente acima/baixo (abs(p-pbot)=1)
--linha = p 
--coluna = d 
-- | Dado o id do bot calcula o numero de jogadores proximos do bot 
jog_perto :: Int -- ^ id bot
          -> Estado -- ^ estado
          -> Int -- ^ n de jog perto 
jog_perto _ (Estado m []) = 0
jog_perto i (Estado m lj@((Jogador p d _ _ _):js)) 
    | (abs(pbot - p) == 1) && (abs(dbot - d) <= 5) = 1 + jog_perto i (Estado m js)
    | otherwise = jog_perto i (Estado m js)
         where (Jogador pbot dbot _ _ _) = jog_i i lj

existe_jog_prox :: Int 
                -> Estado
                -> Bool
existe_jog_prox _ (Estado m []) = False
existe_jog_prox i (Estado m lj@((Jogador p d _ _ _):js)) 
    | (abs(pbot - p) == 1) && (abs(dbot - d) <= 3) = True
    | otherwise = existe_jog_prox i (Estado m js)
        where (Jogador pbot dbot _ _ _) = jog_i i lj


decide_acelera :: Int 
               -> Estado
               -> Bool
decide_acelera i (Estado m lj@((Jogador p d _ _ _):js)) = not ((abs(pbot - p) == 1) && (abs(dbot - d) <= 3))


{-
   jogadas possiveis
       -> disparar cola FEITO 
       -> mudar de pista +- FEITO
               -> se existir um jog muito proximo do bot, o bot muda de pista p evitar uma colisao
                   -> testa se pode mudar de pista e, se possivel, determina se e melhor mudar para cima ou para baixo
                   -> se nao for possivel desacelera 
       -> acelerar
               -> se nao existirem jogadores perto, o bot acelerar
                   -> se existirem, o bot muda de pista 
       -> perto = proximas 5 pecas ????

       -> se existirem jogadores num raio de 5 pecas p frente e 2 para cima/baixo, o bot nao faz nada

       jogs proximos apresentam perigo => mudança na jogada
   
    -}   