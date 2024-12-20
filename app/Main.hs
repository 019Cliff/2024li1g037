module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3
import Eventos
import Desenhar
import Tempo


data EstadoJogo = EstadoJogo
  { mapa :: Mapa
  , moedas :: Int
  , torres :: [Torre]
  , inimigos :: [Inimigo]
  , base :: Base
  }


data EstadoMenu = MenuPrincipal | Jogando EstadoJogo | Sair 

data EstadoApp = EstadoApp
  { estadoAtual :: EstadoMenu
  }

estadoInicialApp :: EstadoApp
estadoInicialApp = EstadoApp { estadoAtual = MenuPrincipal }

estadoInicialJogo :: EstadoJogo
estadoInicialJogo = EstadoJogo
  { mapa = mapa1
  , moedas = creditosBase base1
  , torres = []
  , inimigos = [inimigo1]
  , base = base1
  }

-- Dimensões da janela
janelaLargura, janelaAltura :: Int
janelaLargura = 1920
janelaAltura = 1080

-- Número de blocos do mapa (ajuste conforme o tamanho do seu mapa)
mapaLargura, mapaAltura :: Int
mapaLargura = 40  -- Número de blocos na largura do mapa
mapaAltura = 27   -- Número de blocos na altura do mapa

-- Tamanho de cada bloco
tamanhoBloco :: Float
tamanhoBloco = fromIntegral (min (janelaLargura `div` mapaLargura) (janelaAltura `div` mapaAltura))

-- Definindo a janela
janela :: Display
janela = InWindow "Immutable Towers" (janelaLargura, janelaAltura) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = play janela fundo fr estadoInicialApp desenhaApp reageEventosApp reageTempoApp

desenhaApp :: EstadoApp -> Picture
desenhaApp app = case estadoAtual app of
  MenuPrincipal -> desenhaMenu
  Jogando jogo -> desenhaJogo jogo
  Sair -> Blank

desenhaMenu :: Picture
desenhaMenu = Pictures
  [ Translate (-300) 100 . Scale 0.5 0.5 . Color black $ Text "Menu Principal"
  , Translate (-200) 0 . Scale 0.3 0.3 . Color blue $ Text "1. Jogar (Enter ou Clique)"
  , Translate (-200) (-50) . Scale 0.3 0.3 . Color red $ Text "2. Sair (Esc ou Clique)"
  ]

desenhaJogo :: EstadoJogo -> Picture
desenhaJogo estado = Pictures
  [ desenhaMapa (mapa estado)
  , desenhaTorres (torres estado)
  , desenhaInimigos (inimigos estado)
  , desenhaBase (base estado)
  ]

desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = Pictures $ concatMap desenhaLinha (zip [0..] mapa)
  where
    desenhaLinha (y, linha) = map (desenhaTerreno y) (zip [0..] linha)
    desenhaTerreno y (x, terreno) = 
        Translate (fromIntegral (x * mapaLargura)) (fromIntegral (y * mapaAltura)) (desenhaTerrenoBase terreno)
    desenhaTerrenoBase Relva = Color green (rectangleSolid tamanhoBloco tamanhoBloco)
    desenhaTerrenoBase Terra = Color (makeColorI 165 42 42 255) (rectangleSolid tamanhoBloco tamanhoBloco)
    desenhaTerrenoBase Agua = Color blue (rectangleSolid tamanhoBloco tamanhoBloco)

desenhaTorres :: [Torre] -> Picture
desenhaTorres torres = Pictures $ map desenhaTorre torres
  where
    desenhaTorre torre = Translate x y $ Color red $ Circle 20
      where
        (x, y) = posicaoTorre torre

desenhaInimigos :: [Inimigo] -> Picture
desenhaInimigos inimigos = Pictures $ map desenhaInimigo inimigos
  where
    desenhaInimigo inimigo = Translate x y $ Color black $ Circle 10
      where
        (x, y) = posicaoInimigo inimigo

desenhaBase :: Base -> Picture
desenhaBase base = Translate x y . Color blue $ Circle 20
  where
    (x, y) = posicaoBase base

reageEventosApp :: Event -> EstadoApp -> EstadoApp
reageEventosApp (EventKey (SpecialKey KeyEnter) Down _ _) app =
  case estadoAtual app of
    MenuPrincipal -> app { estadoAtual = Jogando estadoInicialJogo }
    _ -> app
reageEventosApp (EventKey (MouseButton LeftButton) Down _ (x, y)) app =
  case estadoAtual app of
    MenuPrincipal
      | y > -25 && y < 25 && x > -250 && x < 250 -> app { estadoAtual = Jogando estadoInicialJogo }
      | y > -75 && y < -25 && x > -250 && x < 250 -> app { estadoAtual = Sair }
    _ -> app
reageEventosApp (EventKey (SpecialKey KeyEsc) Down _ _) app =
  case estadoAtual app of
    MenuPrincipal -> app { estadoAtual = Sair }
    _ -> app
reageEventosApp evt app = case estadoAtual app of
  Jogando jogo -> app { estadoAtual = Jogando (reageEventosJogo evt jogo) }
  _ -> app

reageTempoApp :: Float -> EstadoApp -> EstadoApp
reageTempoApp dt app = case estadoAtual app of
  Jogando jogo -> app { estadoAtual = Jogando (reageTempoJogo dt jogo) }
  _ -> app

reageEventosJogo :: Event -> EstadoJogo -> EstadoJogo
reageEventosJogo (EventKey (MouseButton LeftButton) Up _ _) estado = estado
reageEventosJogo _ estado = estado

reageTempoJogo :: Float -> EstadoJogo -> EstadoJogo
reageTempoJogo _ estado = estado

verificaFimDeJogo :: EstadoJogo -> Bool
verificaFimDeJogo jogo = terminouJogo Jogo
  { baseJogo = base jogo
  , inimigosJogo = inimigos jogo
  , mapaJogo = mapa jogo
  , torresJogo = torres jogo
  , lojaJogo = []
  , portaisJogo = []
  }
