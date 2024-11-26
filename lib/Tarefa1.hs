{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425


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
validaPortal :: [Portal] -> Bool
validaPortal portais = undefined
      where
            validaOndaPortal :: Portal -> Bool 
            validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal) 
            
            ondaSemInimigos :: Onda -> Bool 
            ondaSemInimigos onda = null (inimigosOnda onda)

            minimoPortal :: [Portal] -> Bool
            minimoPortal portais = not (null portais)

            --Verifica se todos os portais estão posicionados sobre terra.
            posicionadoEmTerra :: Mapa -> [Portal] -> Bool
            posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais

                                                                 
            naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
            naoSobrepostosTorreBase posicoes base torres portais mapa =
                  posicionadoEmRelvaTorre mapa torres &&
                  posicionadoEmTerraBase mapa base &&
                  verificaPosicaoTorreEmPortal mapa torres portais &&
                  verificaPosicaoBaseEmPortal mapa base portais
                        where
                              -- Verifica se todas as torres estão posicionadas em terrenos de Relva
                              posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
                              posicionadoEmRelvaTorre mapa torres =
                                    all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres
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

            caminhoPortalBase :: [Portal] -> Base -> Mapa -> Bool
            caminhoPortalBase portais base mapa = undefined


validaInimigo :: Inimigo -> Bool
validaInimigo inimigo = undefined

validaTorre :: Torre -> Bool
validaTorre = undefined

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

