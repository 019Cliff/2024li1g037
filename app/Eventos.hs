module Eventos where

import Data.List
import GHC.Float (int2Float)
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import Tarefa1

reage :: Event -> ImmutableTowers -> IO ImmutableTowers
-- Navegação do menu principal (setas)
reage (EventKey (SpecialKey KeyRight) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = MenuInicial Creditos}
    MenuInicial Creditos -> return e {modo = MenuInicial Sair}
    _ -> return e
reage (EventKey (SpecialKey KeyLeft) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Creditos -> return e {modo = MenuInicial Jogar}
    MenuInicial Sair -> return e {modo = MenuInicial Creditos}
    _ -> return e
-- Entrar no jogo
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = EmJogo}
    MenuInicial Sair -> error "Sair selecionado"
    MenuInicial Creditos -> return e {modo = TutorialFoto}
    MostrarCreditos -> return e {modo = MenuInicial Creditos} -- Caso queira voltar
    _ -> return e
-- Sair do tutorial
reage (EventKey (SpecialKey KeyEsc) Down _ _) e@(ImmutableTowers _ _ TutorialFoto _ _) =
  return e {modo = MenuInicial Creditos}
-- Pausar e retomar jogo
reage (EventKey (Char 'p') Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    EmJogo -> return e {modo = Pausado}
    Pausado -> return e {modo = EmJogo}
    _ -> return e
-- Selecionar torre na loja (clicar quando não há torre selecionada)
reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ Nothing) = do
  let itensLoja = zip [0 ..] (lojaJogo jogo)
      itemClicado = find (\(i, _) -> lojaClicada mx my i) itensLoja
  case itemClicado of
    Just (_, (preco, torre)) ->
      if creditosBase (baseJogo jogo) >= preco
        then return estado {torreSelecionada = Just torre}
        else return estado
    Nothing -> return estado

-- Colocar torre no mapa (clicar quando há torre selecionada)
reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ (Just torre)) = do
  let posX = floor $ (mx + (larguraJanela / 2)) / int2Float pixeis
      posY = floor $ ((alturaJanela / 2) - my) / int2Float pixeis
      novaPosicao = (fromIntegral posX, fromIntegral posY)
      novaTorre = torre {posicaoTorre = novaPosicao}
      novasTorres = novaTorre : torresJogo jogo

  if terrenoPorPosicao novaPosicao (mapaJogo jogo) == Just Relva
    && notElem novaPosicao (map posicaoTorre (torresJogo jogo))
    then do
      putStrLn $ "Torre colocada em: " ++ show novaPosicao
      return
        estado
          { jogo =
              jogo
                { torresJogo = novasTorres,
                  baseJogo =
                    (baseJogo jogo)
                      { creditosBase = creditosBase (baseJogo jogo) - precoTorre torre (lojaJogo jogo)
                      }
                },
            torreSelecionada = Nothing
          }
    else do
      putStrLn "Posição inválida para torre."
      return estado

-- Caso padrão para qualquer outro evento
reage _ estado = return estado

precoTorre :: Torre -> Loja -> Int
precoTorre torre loja =
  case find (\(_, t) -> tempoTorre t == tempoTorre torre) loja of
    Just (preco, _) -> preco
    Nothing -> 0

-- Função auxiliar para detectar cliques na loja
lojaClicada :: Float -> Float -> Int -> Bool
lojaClicada mx my indice =
  let posX = fromIntegral indice * 150 - 945
      posY = -300
      largura = 150
      altura = 100
   in mx >= posX && mx <= (posX + largura) && my >= posY && my <= (my + altura)
