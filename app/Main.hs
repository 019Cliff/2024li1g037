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

-- Estado do Jogo
data EstadoJogo = EstadoJogo
  { mapa :: Mapa
  , moedas :: Int
  , torres :: [Torre]
  , inimigos :: [Inimigo]
  , base :: Base
  }

-- Estado do Menu
data EstadoMenu = MenuPrincipal | Jogando EstadoJogo | Jogar |  Sair 

-- Estado Geral da Aplicação
data EstadoApp = EstadoApp
  { estadoAtual :: EstadoMenu
  , imgFundo :: Picture
  , imgJogar :: Picture
  }

-- Estado inicial da aplicação
estadoInicialApp :: EstadoApp
estadoInicialApp = EstadoApp
  { estadoAtual = MenuPrincipal
  , imgFundo = undefined -- Imagem carregada de fundo
  , imgJogar = undefined -- Imagem carregada do botão jogar
  }

-- Estado inicial do jogo
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

-- Número de blocos no mapa
mapaLargura, mapaAltura :: Int
mapaLargura = 40
mapaAltura = 27

-- Tamanho de cada bloco
tamanhoBloco :: Float
tamanhoBloco = fromIntegral (min (janelaLargura `div` mapaLargura) (janelaAltura `div` mapaAltura))

-- Configuração da janela
janela :: Display
janela = InWindow "Immutable Towers" (janelaLargura, janelaAltura) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

-- Carregar imagem de fundo do menu
fundoMenu :: IO Picture
fundoMenu = loadBMP "/home/cliff/2024li1g037/app/fundo_1_.bmp"

iconeJogar :: IO Picture 
iconeJogar = loadBMP "/home/cliff/2024li1g037/app/button.bmp"

-- Função principal
main :: IO ()
main = do
  imgFundo <- fundoMenu
  imgJogar <- iconeJogar
  let estadoInicial = EstadoApp { estadoAtual = MenuPrincipal, imgFundo = imgFundo, imgJogar = imgJogar }
  play janela fundo fr estadoInicial desenhaApp reageEventosApp reageTempoApp

-- Desenho da aplicação
desenhaApp :: EstadoApp -> Picture
desenhaApp app = case estadoAtual app of
  MenuPrincipal -> desenhaMenu (imgFundo app)
  Jogando jogo -> desenhaJogo jogo
  Jogar -> desenhaMenu (imgJogar app)
  Sair -> Blank

desenhaMenu :: Picture -> Picture
desenhaMenu imgFundo = Pictures
  [ Scale escalaX escalaY imgFundo
  , Translate (-800) 200 . Scale 0.8 0.8 . Color black $ Text "Menu Principal"
  , Translate (-800) 50 . Scale 0.5 0.5 . Color (makeColorI 0 0 255 255) $ Text "1. Jogar (Enter ou Clique)"
  , Translate (-800) (-100) . Scale 0.5 0.5 . Color (makeColorI 255 0 0 255) $ Text "2. Sair (Esc ou Clique)"
  ]
  where
    escalaX = fromIntegral janelaLargura / 1920
    escalaY = fromIntegral janelaAltura / 1080

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

-- Reação aos eventos
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
reageEventosApp _ app = app

reageTempoApp :: Float -> EstadoApp -> EstadoApp
reageTempoApp _ app = app

-- Reações específicas do jogo
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
