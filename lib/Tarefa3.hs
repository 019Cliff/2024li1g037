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



{-| A funçao atualizaJogo atualiza o estado do jogo em funçao do tempo, utiliza funçoes que atualizam cada parametro.


== Exemplo

>>>atualizaJogo jogo1 1.0


-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo = jogo { 
    torres = map (atualizaTorre tempo) (torres jogo), 
    inimigos = map (atualizaInimigo tempo) (inimigos jogo) ,
    portais = map (atualizaPortal tempo) (portais jogo),
    base = atualizaBase  (base jogo) tempo
  }

{-| A funçao disparosTorre atualiza a torre gerindo os disparos automaticos e os inimigos atingidos.
== Exemplo

>>>disparosTorre torre1 [inimigo1, inimigo2]
(,)
-}

disparosTorre :: Torre -> [Inimigo] -> (Torre, [Inimigo])
disparosTorre  torre inimigos =
    if  tempoTorre torre <= 0 &&  not null (inimigosNoAlcance inimigos)
    then (atualizaTorre (tempoTorre torre) torre, atacaInimigo torre inimigos)
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

{-| A funçao deInimigoABase verifica que caso exista um percurso do inimigo à base então cria uma lista de direçoes 
caso contrario dá erro.

== Exemplo:

>>>deInimigoABase 


-}
deInimigoABase :: Mapa -> Inimigo -> Base -> [Direcao]
deInimigoABase mapa inimigo base =
    if buscaCaminho mapa inimigo base [] 
    then criarDirecoes mapa inimigo base
    else error "Inimigo perdido!"

{-| A funçao criarDirecoes cria uma lista de direcoes caso exista um percurso 


== Exemplo:

>>>criarDirecoes 

-}

criarDirecoes :: Mapa -> Inimigo -> Base -> [Direcao]
criarDirecoes mapa inimigo base = 
 dePosicoesParaDirecoes posicaoInimigo inimigo percurso
   where 
    percurso = calculaOPercurso mapa posicaoInimigo posicaoBase [] 
    posicaoInimigo = posicaoInimigo inimigo
    posicaoBase = posicaoBase base


 {-|A funçao calculaOPercurso retorna uma lista de posições do percurso a seguir. 

 ==Exemplo

 >>>calculaOPercurso
 ERRO
-} 
calculaOPercurso :: Mapa -> Posicao -> Posicao -> [Posicao] -> [Posicao]
calculaOPercurso mapa atualinimigo posbase visitados
    | atualinimigo == posbase = [atualinimigo] 
    | not (posicaoValida mapa atualinimigo) = [] 
    | atualinimigo `elem` visitados = [] 
    | terrenoPorPosicao atualinimigo mapa /= Just Terra = [] 
    | otherwise =
        let visitados' = atualinimigo : visitados
            vizinhos = adjacentes atualinimigo
            caminhos = map (\pos -> calculaOPercurso mapa pos posbase visitados') vizinhos
            caminhoValido = filter (not . null) caminhos 
        in if null caminhoValido
            then [] 
            else atualinimigo : head caminhoValido 



{-| A funçao dePosicoesParaDirecoes transforma uma posiçao uma direçao.

-}
dePosicaoParaDirecao :: Posicao -> Posicao -> Direcao 
dePosicaoParaDirecao posicao proximaPosicao = 
    case (posicao, proximaPosicao) of
        ((x1, y1), (x2, y2))
           |(y2 > y1) -> Norte
           |(y2 < y1) -> Sul
           |(x2 < x1  ) -> Oeste
           |(x2 > x1 ) -> Este
           | otherwise -> error  "As posições são iguais!"
           
{-|A funçao dePosicoesParaDirecoes transforma posições em direções.


-}
dePosicoesParaDirecoes :: [Posicao] -> [Direcao]
dePosicoesParaDirecoes [] = []
dePosicoesParaDirecoes [x] = []
dePosicoesParaDirecoes [x1, x2] = [dePosicaoParaDirecao x1 x2]
dePosicoesParaDirecoes (x1:x2:xs) = dePosicaoParaDirecao x1 x2 : dePosicaoParaDirecao (x2:xs)

{-| A função movimentaInimigo atualiza uma posicao quando recebe uma direcao.


== Exemplo:

>>>movimenta 

-}

movimenta :: Inimigo -> Direcao -> Posicao
movimenta posicaoInimigo inimigo direcao  = novaposicao
        where 
            novaposicao =  case direcao of  
                            Norte -> (x, y + 1)
                            Sul -> (x, y - 1)
                            Oeste -> (x - 1, y)
                            Este -> (x + 1, y)

{-| A funcao projeteisalteramVelocidade ajusta a velocidade do inimigo considerando os efeitos dos projeteis.

==Exemplo

>>>projeteisalteramVelocidade 

-}

projeteisalteramVelocidade :: Inimigo -> Inimigo
projeteisalteramVelocidade i@Inimigo {velocidadeInimigo = velocidade, projeteisInimigo = projeteisAtivos} = 
    i {velocidadeInimigo = novaVelocidade, projeteisAtivos = novosProjeteis}
    where 
        novaVelocidade = alteraVelocidade (projeteisAtivos)
        novosProjeteis = filtraProjeteisExpirados (map (\projetil-> duracaoReduzida projetil tempo) projeteisAtivos)

{-| A funçao alteraVelocidade altera a velocidade do inimigo conforme o tipo de projetil ativo.

== Exemplo:
>>>alteraVelocidade
-}
alteraVelocidade :: Inimigo -> Projetil -> Inimigo
alteraVelocidade inimigo projetil =
     case tipoProjetil projetil of 
      Gelo -> inimigo { velocidadeInimigo = 0 }
      Fogo -> inimigo 
      Resina -> inimigo {velocidadeInimigo = max 0 (velocidadenormal inimigo - fatorResina)}
 
{-|  A funçao acabaEfeito atualiza a lista de projeteis do inimigo , os projteis de fogo e gelo quando 
 têm duraçoes = Finita 0.0 sao removidos e os efeitos deixam de atuar.

== Exemplo:
>>>acabaEfeito
-}
acabaEfeito :: Inimigo -> Inimigo
acabaEfeito projetilInimigo inimigo = novoInimigo
   where
     novoInimigo = inimigo {projeteisInimigo = filtraProjeteisExpirados (projetilInimigo Inimigo)} 


{-| A funcao duracaoReduzida atualiza a duração do projetil com o decorrer do tempo, sendo no maximo igual a zero.

== Exemplo:
>>>duracaoReduzida projetil1 1.0

-}

duracaoReduzida :: Projetil -> Tempo -> Projetil
duracaoReduzida projetil tempo = projetil {duracaoProjetil = novaDuracao}
  where
     novaDuracao = atualizaProjetilComOTempo projetil tempo


 {-| A funcao atualizaProjetilComOTempo atualiza o projetil (a sua duração) com o passar do tempo .
  
== Exemplo:
>>> atualizaProjetilComOTempo
 -}

atualizaProjetilComOTempo :: Projetil -> Tempo -> Projetil
atualizaProjetilComOTempo projetil tempo =
  case  duracaoProjetil projetil of
    Finita -> Projetil {duracaoProjetil = max 0 (duracaoProjetil - tempo)}
    Infinita -> Projetil {duracaoProjetil = Infinita}

{-| A função filtraProjeteisExpirados filtra de uma lista de projeteis os projeteis com duraçao finita igual a zero.

== Exemplo:
>>>filtraProjeteisExpirados [projetil1, projetil2, projetil4]
[projetil1, projetil2]

-}

filtraProjeteisExpirados :: [Projetil] -> [Projetil] 
filtraProjeteisExpirados projeteis = filter (\p -> duracaoProjetil p \= Finita 0.0 ) projeteis

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

{-| A função atualizaInimigo atualiza o estado do inimigo com base na passagem do tempo, tem em consideraçao
 o seu movimento, projeteis ativos que possam impactar a sua velocidade e a remocao deste efeito caso expire

== Exemplo

>>> atualizainimigo inimigo1 4.0

-}

atualizaInimigo :: Tempo -> Inimigo -> Inimigo
atualizaInimigo tempo inimigo = removeEfeito (projeteisalteramVelocidade (movimentaInimigo tempo inimigo))


{-| A funçao atualizaBase atualiza o estado da base em funçao dos butins que recebe pela derrota dos inimigos no decorrer do tempo.


-}



atualizaBase :: Base -> tempo -> Base
atualizaBase base tempo = adicionaButimAosCreditosInimigos (extraiButins novosInimigos) (base jogo)

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
