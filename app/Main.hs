module Main where
import Tempo
import Desenhar
import Eventos
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers

{- | A função @main@ é o ponto de entrada do programa.
-}
main :: IO ()
main = do
  imgs <- carregarImagens
  playIO janela corFundo frameRate imgs desenha reage reageTempo

{- | A constante @janela@ define o tipo de exibição da janela do jogo. 
-}
janela :: Display
janela = FullScreen

{- | A constante @corFundo@ define a cor de fundo da janela do jogo. 
-}
corFundo :: Color
corFundo = black

{- | A constante @frameRate@ define a taxa de quadros por segundo para o jogo.
-}
frameRate :: Int
frameRate = 60

