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

{-| A funçao atualizaTorre atualiza o parametro tempoTorre em funçao da passagem de tempo, sempre que disparar reseta para cicloTorre da torre,
 caso contrario minimui com o decorrer do tempo. 

== Exemplo

>>> atualizaTorre torre

-}
 
atualizaTorre :: Tempo -> Torre -> Torre
atualizaTorre tempo torre 
    | tempoTorre torre > 0  = torre { tempoTorre = max 0 (tempoTorre torre - tempo)} 
    | otherwise = torre { tempoTorre = cicloTorre torre } 



{-| A funçao atacaInimigos atualiza os níveis de vida dos mesmos, conforme o dano de ataque da torre. 

==Exemplo

>>> atacaInimigos torre [inimigo1, inimigo2, inimigo3]
[inimigo1,inimigo2,inimigo3]

-}


atacaInimigo :: Torre -> [Inimigo] -> [Inimigo]
atacaInimigo torre inimigos = map (atingeInimigo inimigos) (inimigosAtingidos torre inimigos)


{- A funçao inimigosAtingidos seleciona o número de alvos da torre em funçao da sua rajada quando os inimigos estão no alcance desta.

== Exemplo

>>> inimigosAtingidos torre {rajadaTorre = 2, posicaoTorre = }  [inimigo1, inimigo2, inimigo3]
[inimigo1, inimigo2]
-}

inimigosAtingidos ::  Torre -> [Inimigo] -> [Inimigo]
inimigosAtingidos torre inimigos = take (rajadaTorre torre) (inimigosNoAlcance torre inimigos)


















{-| A função inimigoAtingeBase atualiza a vida da base caso esta seja atingida por um inimigo.


== Exemplo

>>>inimigoAtingeBase base inimigo 
(...)

-}

inimigoAtingeBase :: Base -> Inimigo -> Base
inimigoAtingeBase base@Base { vidaBase = vida } 
               inimigo@Inimigo { ataqueInimigo = dano} = base {vidaBase = max 0 (vida - dano)}






{-| A função portalComOndasAtivas filtra as ondas ativas de um portal, ou seja,aquelas cujo parametro entradaOnda seja igual ou inferior
a 0 (zero) e que poderá lançar inimigos. 

== Exemplo

>>> portalComOndasAtivas tempo Onda
...
-}
portalComOndasAtivas :: Portal -> Portal
portalComOndasAtivas portal = portal { ondasPortal = filter (\onda -> entradaOnda onda <= 0 )  (ondasPortal portal) }



{-| Aplica a portalComOndasAtivas listas de portais.

== Exemplo

>>> portaisComOndasAtivas tempo Onda
...
-}
portaisComOndasAtivas :: [Portal] -> [Portal]
portaisComOndasAtivas portais = map (portalComOndasAtivas) portais


{-| A função ordemNaturalDosInimigos determina a ordem de saida dos inimigos de uma onda determinada pela ordem natural da lista 
de inimigos.

== Exemplo

>>> ordemNaturalDosInimigos tempo Onda
...
-}



ordemNaturalDosInimigos :: Onda -> [Inimigo]
ordemNaturalDosInimigos onda = inimigosOnda onda 


{-| A função atualizaOnda atualiza as ondas do portal em funçao do tempo considera os parametros cicloOnda e tempoOnda em funçao da passagem de tempo.

== Exemplo

>>> atualizaOnda tempo Onda
...
-}
 
atualizaOnda :: Tempo -> Onda -> Onda
atualizaOnda tempo onda 
    | tempoOnda onda > 0  = onda { tempoOnda = max 0 (tempoOnda onda - tempo)} 
    | otherwise = onda { tempoOnda = cicloOnda onda }

{-| A função atualizaPortal atualiza os portais em funçao do tempo, aplicando a atualizaOnda na lista de ondas do portal.

== Exemplo

>>> atualizaPortal tempo Onda
...

-}
atualizaPortal :: Tempo -> Portal -> Portal 
atualizaPortal tempo portal = portal { ondasPortal = map (atualizaOnda tempo) (ondasPortal portal) }




