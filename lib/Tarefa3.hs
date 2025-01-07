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

import Tarefa1

torre3 :: Torre
torre3 = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil2 }

projetil4 :: Projetil
projetil4 = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 0.0 }

inimigo4 :: Inimigo
inimigo4 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Fogo]}

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Fogo]}

onda1 :: Onda
onda1 = Onda {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0 }

portal2 :: Portal
portal2 = Portal {posicaoPortal = (0, 3), ondasPortal = [onda1,onda2]}

onda2 :: Onda
onda2 = Onda {inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0 }



{-| A funçao atualizaJogo atualiza o estado do jogo em funçao do tempo.


== Exemplo

>>>atualizaJogo jogo1


-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo = novoJogo
     where 
        novoJogo = undefined

{-| A funçao disparosTorre atualiza a torre gerindo os disparos automaticos e os inimigos atingidos.
== Exemplo

>>>disparosTorre torre1 [inimigo1, inimigo2]
(,)
-}


disparosTorre :: Torre -> [Inimigo] -> (Torre, [Inimigo])
disparosTorre  torre inimigos =
    if  tempoTorre torre <= 0 && inimigosNoAlcance inimigos
    then (atualizaTorre torre, atacaInimigo inimigos)
    else (torre, inimigos)

{-| A funçao atualizaTorre atualiza o parametro tempoTorre em funçao da passagem de tempo, sempre que disparar
reseta para cicloTorre da torre, caso contrario minimui com o decorrer do tempo. 

== Exemplo

>>> atualizaTorre 1.0 torre3
Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil2 }


-}

atualizaTorre :: Tempo -> Torre -> Torre
atualizaTorre tempo torre
    | tempoTorre torre > 0  = torre { tempoTorre = max 0 (tempoTorre torre - tempo)}
    | otherwise = torre { tempoTorre = cicloTorre torre }



{-| A funçao atacaInimigos atualiza os níveis de vida dos mesmos, conforme o dano de ataque da torre. 

==Exemplo

>>> atacaInimigos torre1 [inimigo1, inimigo2, inimigo3]
[inimigo1,inimigo2, inimigo3]

-}


atacaInimigo :: Torre -> [Inimigo] -> [Inimigo]
atacaInimigo torre inimigos = map (atingeInimigo inimigos) (inimigosAtingidos torre inimigos)


{- A funçao inimigosAtingidos seleciona o número de alvos da torre em funçao da sua rajada quando os inimigos estão no alcance desta.

== Exemplo

>>> inimigosAtingidos torre3 [inimigo1, inimigo2, inimigo3]
[inimigo1, inimigo2]
-}

inimigosAtingidos ::  Torre -> [Inimigo] -> [Inimigo]
inimigosAtingidos torre inimigos = take (rajadaTorre torre) (inimigosNoAlcance torre inimigos)




{-| A funcao ajustaVelocidade ajusta a velocidade do inimigo com atençao aos efeitos dos projeteis.

==Exemplo

>>>ajustaVelocidade
...
-}

ajustaVelocidade :: Inimigo -> Velocidade
ajustaVelocidade inimigo = undefined


{-|A função efeitosInimigos aplica os efeitos dos projeteis nos inimigos.

==Exemplo
>>> efeitosInimigos
...


-}

efeitosInimigos :: [Inimigo] -> [Projetil] -> [Inimigo]
 inimigos projeteis = undefined


{-| A funçao extraiButins extraí os butins dos inimigos de uma lista de inimigos e coloca-as numa 
 lista.

 ==Exemplo
>>>extraiButins [inimigo1, inimigo2, inimigo3]
[50, 50, 100]
-}

extraiButins :: [Inimigo] -> [Creditos]
extraiButins inimigos = [ butimInimigo inimigo | inimigo <- inimigos , vidaInimigo inimigo <= 0]

{-| A função adicionaOsCreditosABase devolve uma base com os seus creditos atualizados somando a lista de butins 
dos inimigos extraidos com os creditos da base.

==Exemplo
>>>adicionaOsCreditosABase [50,50,100] base1
Base { posicaoBase = (1, 1), creditosBase = 300, vidaBase = 100.0 }

-}

adicionaOsCreditosABase :: [Creditos] -> Base -> Base
adicionaOsCreditosABase butins base@(Base {creditosBase = credito }) = base{creditosBase = novosCreditos }
     where
       novosCreditos = credito + sum butins


{-| A funçao adicionaButimAosCreditosInimigos combina as duas funçoes de forma a facilitar a soma 
==Exemplo
>>>adicionaButimAosCreditosInimigos [inimigo1, inimigo2] base1 
Base { posicaoBase = (1, 1), creditosBase = 300, vidaBase = 100.0 }
-}
adicionaButimAosCreditosInimigos :: [Inimigo] -> Base -> Base
adicionaButimAosCreditosInimigos inimigos base = adicionaOsCreditosABase (extraiButins inimigos) base



{-|A função retiraInimigoEmFunçaoDaVida retira o inimigo da lista de inimigos ativos se a vida deste for igual ou menor que zero.

==Exemplo
>>>retiraInimigoEmFunçaoDaVida [inimigo1, inimigo4]
[inimigo1]

-}

retiraInimigoEmFunçaoDaVida :: [Inimigo] -> [Inimigo]
retiraInimigoEmFunçaoDaVida = filter (\inimigo -> vida inimigo > 0 )

{-| A função eliminaDaListaDeAtivos retira os inimigos da lista de inimigos quando estes atingem a base.


==Exemplo
>>> eliminaDaListaDeAtivos base1 [inimigo2, inimigo1]
inimigo1

 
-}


eliminaDaListaDeAtivos :: [Inimigo] -> Base -> [Inimigo]
eliminaDaListaDeAtivos inimigos base =
    filter (\inimigo -> not (verificainimigoAtingeBase inimigo base)) inimigos


{-| A função inimigoAtingeBase atualiza a vida da base caso esta seja atingida por um inimigo.


== Exemplo

>>>inimigoAtingeBase base1 inimigo2
Base { posicaoBase = (1, 1), creditosBase = 100, vidaBase = 90.0 }

-}

inimigoAtingeBase :: Base -> Inimigo -> Base
inimigoAtingeBase base@Base { vidaBase = vida }
               inimigo@Inimigo { ataqueInimigo = dano} = base {vidaBase = max 0 (vida - dano)}


{-! A funçao verificainimigoAtingeBase verifica se o inimigo atingiu a base, ou seja, se deu um dano maior q 0.

== Exemplo:
>>> verificainimigoAtingeBase inimigo2 base1
True

-}

verificainimigoAtingeBase :: Inimigo -> Base -> Bool
verificainimigoAtingeBase inimigo base 
   |danoinimigo inimigo > 0 = True
   |otherwise = False

{-| A funçao ajustaDistancia atualiza a distancia percorrida pelo inimigo 
ajustada em função dos efeitos e do parametro velocidadeInimigo 

== Exemplo:

>>> ajustaDistancia 5 2.0

-}

ajustaDistancia :: Distancia -> Tempo -> Distancia
ajustaDistancia distancia tempo = novadistancia
          where 
            novadistancia = tempo * velocidadeinimigo




{-| A função portalComOndasAtivas filtra as ondas ativas de um portal, ou seja,aquelas cujo parametro entradaOnda seja igual ou inferior
a 0 (zero) e que poderá lançar inimigos. 

== Exemplo

>>> portalComOndasAtivas portal2
Portal {portal1 = Portal { posicaoPortal = (0, 3), ondasPortal = [onda1]}

-}
portalComOndasAtivas :: Portal -> Portal
portalComOndasAtivas portal = portal { ondasPortal = filter (\onda -> entradaOnda onda <= 0 )  (ondasPortal portal) }



{-| Aplica a portalComOndasAtivas listas de portais.

== Exemplo

>>> portaisComOndasAtivas [portal1, portal2]
[portal1,Portal {portal1 = Portal { posicaoPortal = (0, 3), ondasPortal = [onda1]}]

-}
portaisComOndasAtivas :: [Portal] -> [Portal]
portaisComOndasAtivas = map (portalComOndasAtivas)


{-| A função ordemNaturalDosInimigos determina a ordem de saida dos inimigos de uma onda determinada pela ordem natural da lista 
de inimigos.

== Exemplo

>>> ordemNaturalDosInimigos onda1
[inimigo2,inimigo3]
-}



ordemNaturalDosInimigos :: Onda -> [Inimigo]
ordemNaturalDosInimigos = inimigosOnda


{-| A função atualizaOnda atualiza as ondas do portal em funçao do tempo considera os parametros cicloOnda 
e tempoOnda em funçao da passagem de tempo.

== Exemplo

>>> atualizaOnda 4.0 onda1
Onda {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 2.0, entradaOnda = 5.0 } 
-}

atualizaOnda :: Tempo -> Onda -> Onda
atualizaOnda tempo onda
    | tempoOnda onda > 0  = onda { tempoOnda = max 0 (tempoOnda onda - tempo)}
    | otherwise = onda { tempoOnda = cicloOnda onda }

{-| A função atualizaPortal atualiza os portais em funçao do tempo, aplicando a atualizaOnda na lista de ondas do portal.

== Exemplo

>>> atualizaPortal 2.0 portal2
Portal {portal1 = Portal { posicaoPortal = (0, 3), ondasPortal = [Onda = {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 }
,Onda = {inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 } ]}

-}
atualizaPortal :: Tempo -> Portal -> Portal
atualizaPortal tempo portal = portal { ondasPortal = map (atualizaOnda tempo) (ondasPortal portal) }


