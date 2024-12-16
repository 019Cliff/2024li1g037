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
-- Representa o estado do jogo.
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

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

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
