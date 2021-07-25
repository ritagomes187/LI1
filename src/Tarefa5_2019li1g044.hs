-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11920
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Tarefa0_2019li1g044


data Picture = Polygon Path | Color Picture Color | Text String | Bitmap BitmapData | Translate Float Float Picture | Pictures [Picture]
data Display = InWindow String (Int, Int) (Int, Int) | FullScreen
data Estado = Estado 
type Path = [Point]
type Point = (Float, Float)
type Mapa = [[Peca]] 

--loadBMP :: File Path -> IO Picture

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

-- * Estado Jogo
desenhaEstado :: Estado -> [Picture] -> [Picture] 
desenhaEstado (Estado m ((Jogador _ d _ _ _): _)) textures = desenha_mapa mt (-1000-d1) (600+d2) textures
                                                         where mt = transpose m
                                                               d1 = realToFrac d 
                                                               d2 = (realToFrac d) * 0.6

desenha_mapa :: Mapa -> Float -> Float -> [Picture] -> [Picture]
desenha_mapa _ _ _ _ = []
desenha_mapa (p:ps) x y textures = (desenha_pista p x y textures) ++ (desenha_mapa ps (x+150) (y-90) textures)

desenha_pista :: Pista -> Float -> Float -> [Picture] -> Picture
desenha_pista [] _ _ _ = []
desenha_pista ((Recta Boost _):ps) x y (cola:boost:terra:lama:relva:[]) =  (Translate x+LARGURADAPECA y boost)  
desenha_pista ((Recta Lama _):ps) x y (cola:boost:terra:lama:relva:[]) = Translate x y lama
desenha_pista ((Recta Cola _):ps) x y (cola:boost:terra:lama:relva:[]) = Translate x y cola 
desenha_pista ((Recta Relva _):ps) x y (cola:boost:terra:lama:relva:[]) = Translate x y relva 
desenha_pista ((Recta Terra _):ps) x y (cola:boost:terra:lama:relva:[]) = Translate x y terra 
desenha_pista ((Rampa Boost _):ps) x y (rcola:rboost:rterra:rlama:rrelva:[]) =  Translate x y rboost  
desenha_pista ((Rampa Lama _):ps) x y (rcola:rboost:rterra:rlama:rrelva:[]) = Translate x y rlama
desenha_pista ((Rampa Cola _):ps) x y (rcola:rboost:rterra:rlama:rrelva:[]) = Translate x y rcola 
desenha_pista ((Rampa Relva _):ps) x y (rcola:rboost:rterra:rlama:rrelva:[]) = Translate x y rrelva 
desenha_pista ((Rampa Terra _):ps) x y (rcola:rboost:rterra:rlama:rrelva:[]) = Translate x y rterra 

--desenha_peca :: Peca -> Float -> Float -> [Picture] -> [Picture]
--desenha_peca p x y textures = 

-- | Percorre um mapa e altera os valores de x y no ecra
move_mapa :: Mapa -> (Float, Float) -> (Float, Float)
move_mapa [] (x, y) = (x, y)
move_mapa (p:ps) (x, y) = move_pista p (x, y) ++ move_mapa ps (x, y-1)

move_pista :: Pista --
		   -> (Float, Float) -- (x, y)
		   -> (Float, Float) -- (x, y)
move_pista [] (x, y) = (x, y)
move_pista (p:ps) (x, y) = desenha_peca 

estadoInicial :: Estado
estadoInicial = (Estado stringParaMapa mapa1) [(Jogador 0 2 2 2 (Chao True))] )


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s -- ignora qualquer outro evento
--reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,y) = (x,y+5)
--reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,y) = (x,y-5)
--reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y) = (x-5,y)
--reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (x+5,y)


reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado m (Jogador a b c d e) : t) = (Estado m (Jogador a (b+2) c d e) : t)

-- * Estado Gloss

type EstadoGloss = (Estado, [Picture])

estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial li = (estadoInicial, li)

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e,li) = estado
                           where textures = listaEntre2Indices 0 5 
                                 estado = Pictures [desenhaEstado e textures]

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss --tarefa2
reageEventoGloss ev (e,li) = (reageEvento ev e,li)

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss -- tarefa 4 
reageTempoGloss t (e,li) = (reageTempo t e, li)

fr :: Int -- frame rate
fr = 150

dm :: Display -- Janela de visualizacao
dm = FullScreen
	--InWindow "Novo Jogo" (400, 400) (0, 0)

main :: IO ()
main = do 
    Just cola <- loadJuicy "cola.jpg"
    Just boost <- loadJuicy "boost.svg"
    Just terra <- loadJuicy "terra.jpg"
    Just lama <- loadJuicy "terra.jpg"
    Just relva <- loadJuicy "relva.jpg"
    play dm                                                   -- janela onde irá correr o jogo
        (greyN 0.5)                                           -- côr do fundo da janela
        fr                                                    -- frame rate
        (estadoGlossInicial [cola, boost, terra, lama, relva, rcola, rboost, rterra, rlama, rrelva]) -- estado inicial
        desenhaEstadoGloss                                    -- desenha o estado do jogo
        reageEventoGloss                                      -- reage a um evento
        reageTempoGloss                                       -- reage ao passar do tempo


