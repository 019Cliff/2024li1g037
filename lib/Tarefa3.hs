{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425

import Tarefa2 

{-| A funçao atualizaJogo atualiza o estado do jogo em funçao do tempo.


== Exemplo

>>>
-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo = undefined

{-| A funçao disparosTorre atualiza a torre gerindo os disparos automaticos e os inimigos atingidos

-}


disparosTorre :: Torre -> [Inimigo] -> ([Inimigo], Torre)
disparosTorre  torre inimigos = 
    if  tempoTorre <= 0 torre && inimigosNoAlcance inimigos
    then (atacaInimigo inimigos, atualizaTorre torre) 
    else (inimigos, torre)

{-| A funçao atualizaTorre atualiza o parametro tempoTorre sempre que disparar reseta para cicloTorre da torre, caso contrario minimui 
com o decorrer do tempo.

== Exemplo

>>> atualizaTorre torre


atualizaTorre :: Torre -> Torre
atualizaTorre torre = 
    | tempoTorre torre > 0  = torre tempoTorre - tempo  
    | otherwise = tempoTorre torre == cicloTorre torre
 

Decrementar o valor de tempoTorre com o passar do tempo, garantindo que a torre esteja pronta para disparar quando este valor chegar a 0 ou menos.
Resetar o valor de tempoTorre para cicloTorre sempre que a torre disparar, como parte do mecanismo de "cooldown
-}
 
atualizaTorre :: Torre -> Torre
atualizaTorre torre 
    | tempoTorre torre > 0  = torre { tempoTorre = max 0 (tempoTorre - tempo)} 
    | otherwise tempoTorre torre == 0 = cicloTorre torre

{-| A funçao atacaInimigos atualiza os níveis de vida dos mesmos, conforme o dano de ataque da torre. 

==Exemplo

>>> atacaInimigos torre [inimigo1, inimigo2, inimigo3]
[inimigo1,inimigo2,inimigo3]

-}
atacaInimigo :: Torre -> [Inimigo] -> [Inimigo]
atacaInimigo torre inimigos =  atingeInimigos inimigos (inimigosAtingdos torre inimigos)


{- A funçao inimigosAtingidos seleciona o número de alvos da torre em funçao da sua rajada quando os inimigos estão no alcance desta.

== Exemplo

>>> inimigosAtingidos torre {rajadaTorre = 2, posicaoTorre = }  [inimigo1, inimigo2, inimigo3]
[inimigo1, inimigo2]
-}

inimigosAtingidos ::  Torre -> [Inimigo] -> [Inimigo]
inimigosAtingidos torre inimigos = take (rajadaTorre torre) (inimigosNoAlcance torre inimigos)




