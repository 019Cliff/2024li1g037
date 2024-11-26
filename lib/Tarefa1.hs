{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425
import Data.List

mapaJogo  :: Mapa
mapaJogo = [[t, t, r, a, a, a],
            [r, t, r, a, r, r],
            [r, t, r, a, r, t],
            [r, t, r, a, r, t],
            [r, t, t, t, t, t],
            [a, a, a, a, r, r]
            ]
      where
            t = Terra
            r = Relva
            a = Agua

terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoPorPosicao (x, y) mapa =
                                          let linha = mapa !! (floor y) -- Converte Float para índice de linha
                                              terreno = linha !! (floor x) -- Converte Float para índice de coluna
                                           in Just terreno

validaJogo :: Jogo -> Bool
validaJogo = undefined

--Verifica se todos os portais do jogo têm ondas sem inimigos no início
validaPortais :: Mapa -> Base -> [Torre] -> [Portal] -> Bool
validaPortais mapa base torres portais =
    minimoPortal portais && 
    posicionadoEmTerra mapa portais &&
    caminhoPortalBase mapa (map posicaoPortal portais) (posicaoBase base) && 
    naoSobrepostosTorreBase (map posicaoPortal portais) base torres portais mapa && 
    maximoOndaPorPortal portais 

     
validaOndaPortal :: Portal -> Bool 
validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal) 
            
ondaSemInimigos :: Onda -> Bool 
ondaSemInimigos onda = null (inimigosOnda onda)

minimoPortal :: [Portal] -> Bool
minimoPortal portais = not (null portais)

 --Verifica se todos os portais estão posicionados sobre terra.
posicionadoEmTerra :: Mapa -> [Portal] -> Bool
posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais

-- Verifica se todas as torres estão posicionadas em terrenos de Relva
posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
posicionadoEmRelvaTorre mapa torres = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres
                 
naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase posicoes base torres portais mapa =
      posicionadoEmRelvaTorre mapa torres &&
      posicionadoEmTerraBase mapa base &&
      verificaPosicaoTorreEmPortal mapa torres portais &&
      verificaPosicaoBaseEmPortal mapa base portais
            where
                  -- Verifica se a base está posicionada em terreno de Terra
                  posicionadoEmTerraBase :: Mapa -> Base -> Bool
                  posicionadoEmTerraBase mapa base =
                        terrenoPorPosicao (posicaoBase base) mapa == Just Terra

                  -- Verifica se torres não estão sobre portais
                  verificaPosicaoTorreEmPortal :: Mapa -> [Torre] -> [Portal] -> Bool 
                  verificaPosicaoTorreEmPortal mapa torres portais =
                        all (\torre -> posicaoTorre torre `notElem` map posicaoPortal portais) torres

                  -- Verifica se a base não está sobre portais
                  verificaPosicaoBaseEmPortal :: Mapa -> Base -> [Portal] -> Bool 
                  verificaPosicaoBaseEmPortal mapa base portais =
                        posicaoBase base `notElem` map posicaoPortal portais
            
-- Limita o número de ondas por portal para apenas 1
maximoOndaPorPortal :: [Portal] -> Bool 
maximoOndaPorPortal portais = all (\portal -> length (ondasPortal portal) <= 1) portais 

caminhoPortalBase :: Mapa -> [Posicao] -> Posicao -> Bool
caminhoPortalBase mapa portais base = any (\portal -> buscaCaminho mapa portal base []) portais

-- Busca recursiva para verificar se há um caminho
buscaCaminho :: Mapa -> Posicao -> Posicao -> [Posicao] -> Bool
buscaCaminho mapa atual destino visitados
                                          | atual == destino = True
                                          | not (posicaoValida mapa atual) = False
                                          | atual `elem` visitados = False
                                          | terrenoPorPosicao atual mapa /= Just Terra = False
                                          | otherwise =
                                                let visitados' = atual : visitados
                                                    vizinhos = adjacentes atual
                                                in any (\pos -> buscaCaminho mapa pos destino visitados') vizinhos

-- Obter posições adjacentes
adjacentes :: Posicao -> [Posicao]
adjacentes (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

            -- Verifica se uma posição está dentro dos limites do mapa
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (x, y) = floor x >= 0 && floor x < length mapa && floor y >= 0 && floor y < length (head mapa) -- floor x converte x para Int

-- Recupera o terreno em uma posição específica
recuperaPosicao :: Posicao -> Mapa -> Maybe Terreno
recuperaPosicao (x, y) mapa
                              | posicaoValida mapa (x, y) = Just ((mapa !! floor x) !! floor y)
                              | otherwise = Nothing
validaInimigos :: Inimigo -> Bool
validaInimigos = undefined

validaTorres :: Mapa -> [Torre] -> Bool
validaTorres mapa torres =
    todasEmRelva mapa torres && 
    alcancesPositivos torres &&
    rajadasPositivas torres &&  
    ciclosNaoNegativos torres && 
    naoSobrepostas torres 

    where
        --Verifica se todas as torres estão posicionadas sobre terrenos de Relva
        todasEmRelva :: Mapa -> [Torre] -> Bool
        todasEmRelva mapa = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva)

        --Verifica se todos os alcances das torres são positivos
        alcancesPositivos :: [Torre] -> Bool
        alcancesPositivos = all (\torre -> alcanceTorre torre > 0)

        --Verifica se todas as rajadas das torres são positivas
        rajadasPositivas :: [Torre] -> Bool
        rajadasPositivas = all (\torre -> rajadaTorre torre > 0)

        --Verifica se todos os ciclos das torres são não negativos
        ciclosNaoNegativos :: [Torre] -> Bool
        ciclosNaoNegativos = all (\torre -> cicloTorre torre >= 0)

        --Verifica se as torres não estão sobrepostas (posições únicas)
        naoSobrepostas :: [Torre] -> Bool
        naoSobrepostas torres =
            let posicoes = map posicaoTorre torres
            in length posicoes == length (nub posicoes)

validaBase :: Base -> Creditos -> [Portal] -> [Torre] -> Mapa -> Bool
validaBase base creditos portais torres mapa =
                                                creditoPositivo creditos &&
                                                naoSobrepostoTorrePortal portais torres base mapa &&
                                                posicionadoEmTerraBase mapa base
                                                where
-- Verifica se os créditos são positivos
creditoPositivo :: Creditos -> Bool
creditoPositivo creditos = creditos >= 0

-- Verifica que a base não está sobreposta a torres ou portais
naoSobrepostoTorrePortal :: [Portal] -> [Torre] -> Base -> Mapa -> Bool
naoSobrepostoTorrePortal portais torres base mapa =
            verificaPosicaoTorreEmBase mapa torres base &&
            verificaPosicaoPortalBase mapa portais base

-- Verifica que nenhuma torre está sobre a base
verificaPosicaoTorreEmBase :: Mapa -> [Torre] -> Base -> Bool 
verificaPosicaoTorreEmBase mapa torres base =
            all (\torre -> posicaoTorre torre /= posicaoBase base) torres

-- Verifica que nenhum portal está sobre a base
verificaPosicaoPortalBase :: Mapa -> [Portal] -> Base -> Bool
verificaPosicaoPortalBase mapa portais base =
            all (\portal -> posicaoPortal portal /= posicaoBase base) portais

-- Verifica que a base está posicionada em um terreno de tipo "Terra"
posicionadoEmTerraBase :: Mapa -> Base -> Bool
posicionadoEmTerraBase mapa base = 
            terrenoPorPosicao (posicaoBase base) mapa == Just Terra