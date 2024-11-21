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

validaJogo :: Jogo -> Bool
validaJogo = undefined

--  Verifica se todos os portais do jogo têm ondas sem inimigos no início
validaPortal :: [Portal] -> Bool
validaPortal portais = all validaOndaPortal portais 
      where
            validaOndaPortal :: Portal -> Bool 
            validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal) 
            
            ondaSemInimigos :: Onda -> Bool 
            ondaSemInimigos onda = null (inimigosOnda onda)

            minimoPortal :: [Portal] -> Bool
            minimoPortal portais = not (null portais)

       --   Verifica se todos os portais estão posicionados sobre terra.
            posicionadoEmTerra :: Mapa -> [Portal] -> Bool
            posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais

            -- Obtém o terreno numa posição específica do mapa.
            terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
            terrenoPorPosicao (x, y) mapa =
                                          let linha = mapa !! (floor y) -- Converte Float para índice de linha
                                              terreno = linha !! (floor x) -- Converte Float para índice de coluna
                                           in Just terreno
                                                                 

validaInimigo :: Inimigo -> Bool
validaInimigo = undefined

validaTorre :: Torre -> Bool
validaTorre = undefined

validaBase :: Base -> Bool
validaBase = undefined 
            

