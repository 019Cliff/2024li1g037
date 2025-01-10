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
import Data.Function ( (&) )

-- Definição de variáveis exemplo
torre3 :: Torre
torre3 = Torre { posicaoTorre = (3, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil1 }

projetil4 :: Projetil
projetil4 = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 0.0 }


inimigo2 :: Inimigo
inimigo2 = Inimigo { posicaoInimigo = (1, 1), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

inimigo3 :: Inimigo
inimigo3 = Inimigo { posicaoInimigo = (2, 2), direcaoInimigo = Sul, velocidadeInimigo = 2.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 100, projeteisInimigo = []}

inimigo4 :: Inimigo
<<<<<<< HEAD
inimigo4 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [ Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}]}

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}]}
=======
inimigo4 = Inimigo { posicaoInimigo = (1, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}
>>>>>>> temp

onda1 :: Onda
onda1 = Onda { inimigosOnda = [inimigo4], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0 }

portal2 :: Portal
portal2 = Portal { posicaoPortal = (0, 3), ondasPortal = [onda1, onda2] }

onda2 :: Onda
onda2 = Onda { inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0 }

jogo2 :: Jogo
jogo2 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre3], lojaJogo = [(50, torre3)]}

{-|
Atualiza o estado do jogo: inimigos, torres, portais e base.

<<<<<<< HEAD
{-| A funçao atualizaJogo atualiza o estado do jogo em funçao do tempo, utiliza funçoes que atualizam cada parametro.


== Exemplo

>>>atualizaJogo 2.0 jogo1

=======
== Exemplos:
>>> atualizaJogo 5 [torre3] [inimigo4] portal2 base1
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 200}
>>>>>>> temp

>>> atualizaJogo 10 [torre3] [inimigo4, inimigo5] portal2 base1
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 250}
-}

<<<<<<< HEAD
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo = jogo {
    torresJogo = torresAtualizadas tempo (torresJogo jogo) (inimigosJogo jogo),
    inimigosJogo = map (atualizaInimigo tempo) (retiraInimigoEmFunçaoDaVida (inimigosJogo jogo)) ,
    portaisJogo = map (atualizaPortal tempo) (portaisJogo jogo),
    baseJogo = atualizaBase (baseJogo jogo) tempo (inimigosJogo jogo),
    lojaJogo = lojaJogo jogo,
    mapaJogo = mapaJogo jogo

  }

{-| A funçao disparosTorre atualiza a torre gerindo os disparos automaticos e os inimigos atingidos, trata de detetar e
atirar nos inimigos.
== Exemplo

>>>disparosTorre torre3 [inimigo1, inimigo2]
(Torre ,inimigo2)
-}

disparosTorre :: Torre -> [Inimigo] -> (Torre, [Inimigo])
disparosTorre  torre inimigos =
    if  tempoTorre torre <= 0 &&  not (null (inimigosNoAlcance torre inimigos))
    then (atualizaTorre (tempoTorre torre) torre, atacaInimigo torre inimigos)
    else (torre, inimigos)


{-| A funçao atualizaTorre atualiza o parametro tempoTorre em funçao da passagem de tempo, sempre que disparar
reseta para cicloTorre da torre, caso contrario diminui com o decorrer do tempo. 

== Exemplo

>>> atualizaTorre 1.0 torre3
Torre { posicaoTorre = (3, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil1 }

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
atacaInimigo torre inimigos = map (atingeInimigo torre) (inimigosAtingidos torre inimigos)


{-| A funçao torresAtualizadas atualiza o estado da torre, reunindo todas as funçoes anteriores.

== Exemplos:

>>> torresAtualizadas  2.0 [torre1, torre2] [inimigo1, inimigo3]
[]

-}


torresAtualizadas :: Tempo -> [Torre] -> [Inimigo] -> [Torre]
torresAtualizadas tempo torres inimigos = map (atualizaTorre tempo) (map (disparosTorre2 inimigos) torres)
  where
     disparosTorre2 inimigos torre = fst (disparosTorre torre inimigos)


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

>>>deInimigoABase mapa1 inimigo1 base1 
[Este, Norte]


-}
deInimigoABase :: Mapa -> Inimigo -> Base -> [Direcao]
deInimigoABase mapa inimigo base =
    if buscaCaminho mapa (posicaoInimigo inimigo) (posicaoBase base) []
    then criarDirecoes mapa inimigo base
    else error "Inimigo perdido!"


{-| A funçao criarDirecoes cria uma lista de direcoes caso exista um percurso 


== Exemplo:

>>>criarDirecoes 

-}

criarDirecoes :: Mapa -> Inimigo -> Base -> [Direcao]
criarDirecoes mapa inimigo base =
 dePosicoesParaDirecoes (posicaoInimigo inimigo : percurso)
   where
    percurso = calculaOPercurso mapa (posicaoInimigo inimigo) (posicaoBase base) []


 {-|A funçao calculaOPercurso retorna uma lista de posições do percurso a seguir. 

 ==Exemplo

 >>>calculaOPercurso mapa posicoa posicao [posicao]
 []
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

== Exemplo:
>>>dePosicaoParaDirecao (0,0) (0,1)
Norte

-}
dePosicaoParaDirecao :: Posicao -> Posicao -> Direcao
dePosicaoParaDirecao posicao proximaPosicao =
    case (posicao, proximaPosicao) of
        ((x1, y1), (x2, y2))
           | y2 > y1 -> Norte
           | y2 < y1  -> Sul
           | x2 < x1  -> Oeste
           | x2 > x1  -> Este
           | otherwise -> error  "As posições são iguais!"

{-|A funçao dePosicoesParaDirecoes transforma posições em direções.

== Exemplo:
>>> dePosicoesParaDirecoes [(0,0), (0,1), (2,1)]
[Norte, Este]
-}
dePosicoesParaDirecoes :: [Posicao] -> [Direcao]
dePosicoesParaDirecoes [] = []
dePosicoesParaDirecoes [_] = []
dePosicoesParaDirecoes (p1:p2:ps) = dePosicaoParaDirecao p1 p2 : dePosicoesParaDirecoes (p2:ps)

{-| A função movimentaInimigo atualiza uma posicao quando recebe uma direcao.


== Exemplo:

>>>movimenta (2,1) Este
(3,1)
-}

movimenta :: Posicao -> Direcao -> Posicao
movimenta (x, y) direcao =
      case direcao of
         Norte -> (x, y + 1)
         Sul   -> (x, y - 1)
         Oeste -> (x - 1, y)
         Este  -> (x + 1, y)


{-| A funçao atualizaMovimentoInimigo aplica a funçao movimenta a uma lista de posicoes. Assim o inimigo vai seguir cada direcao 
atualizando sempre a sua posicao antiga.

== Exemplo:

>>> atualizaMovimentoInimigo inimigo1 [Norte, Sul, Este]
inimigo { posicaoInimigo = (1,0)}
-}

atualizaMovimentoInimigo :: Inimigo -> [Direcao] -> Inimigo
atualizaMovimentoInimigo inimigo [] = inimigo
atualizaMovimentoInimigo inimigo (d:ds) = atualizaMovimentoInimigo (inimigo { posicaoInimigo = movimenta (posicaoInimigo inimigo) d }) ds

{-| A funcao projeteisalteramVelocidade atualiza a o inimigo em função dos efeitos que os projeteis ativos nele têm na sua velocidade, considera a
atualização dos projeteis, reduzindo o tempo das suas durações e caso eles expirem

==Exemplo

>>>projeteisalteramVelocidade 2.0 inimigo1
inimigo {velocidadeinimigo = , projeteisInimigo}
-}

projeteisalteramVelocidade :: Inimigo -> Tempo -> Inimigo
projeteisalteramVelocidade inimigo@Inimigo {velocidadeInimigo = velocidade, projeteisInimigo = projeteis} tempo = 
 inimigo {velocidadeInimigo = novaVelocidade, projeteisInimigo = projeteisatualizados}
   where
    novaVelocidade = alteraVelocidade inimigo (head projeteisatualizados)
    projeteisatualizados = filtraProjeteisExpirados (duracaoReduzida projeteis tempo) 



{-| A funçao alteraVelocidade altera a velocidade do inimigo conforme o tipo de projetil ativo.

== Exemplo:
>>>alteraVelocidade
-}
alteraVelocidade :: Inimigo -> Projetil -> Inimigo
alteraVelocidade inimigo projetil =
     case tipoProjetil projetil of
      Gelo -> inimigo { velocidadeInimigo = 0 }
      Fogo -> inimigo
      Resina -> inimigo {velocidadeInimigo = max 0 (velocidadeNormal - fatorResina)}
  where
    velocidadeNormal = velocidadeInimigo inimigo
    fatorResina = 2.0

{-|  A funçao acabaEfeito atualiza a lista de projeteis do inimigo , os projeteis de fogo e gelo quando 
 têm duraçoes = Finita 0.0 sao removidos e os efeitos deixam de atuar.

== Exemplo:
>>>acabaEfeito
-}

acabaEfeito :: Inimigo -> Inimigo
acabaEfeito inimigo@Inimigo {projeteisInimigo = projeteis}  = inimigo {projeteisInimigo = filtraProjeteisExpirados (projeteisInimigo inimigo)}

{-| A funcao duracaoReduzida atualiza a duraçao de projeteis numa lista, utilizando a funçao atualizaProjetilComOTempo.
== Exemplo:
>>>duracaoReduzida [projetil1,projetil2] 1.0
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0 }, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}]
-}

duracaoReduzida :: [Projetil] -> Tempo -> [Projetil]
duracaoReduzida projeteis tempo = map (atualizaProjetilComOTempo tempo) projeteis
=======
atualizaJogo :: Tempo -> [Torre] -> [Inimigo] -> Portal -> Base -> Base
atualizaJogo tempo torres inimigos portal base =
  atualizaBase tempo (base {creditosBase = creditosBase base + butim}) 
  where
    butim = sum [butimInimigo i | i <- inimigosDerrotados]
    inimigosDerrotados = filter (\i -> vidaInimigo i <= 0) inimigos
>>>>>>> temp

{-|
Atualiza a base: perde vida se inimigos atingem ela.

<<<<<<< HEAD
 {-| A funcao atualizaProjetilComOTempo atualiza o projetil (a sua duração) com o passar do tempo, sendo 
 no maximo igual a zero
  
== Exemplo:
>>> atualizaProjetilComOTempo projetil1 3.0
Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0 }
 -}

atualizaProjetilComOTempo :: Projetil -> Tempo -> Projetil
atualizaProjetilComOTempo projetil tempo =
  case duracaoProjetil projetil of
       Finita t -> projetil {duracaoProjetil = Finita (max 0 ( t - tempo))}
       Infinita -> projetil


{-| A função filtraProjeteisExpirados filtra de uma lista de projeteis os projeteis com duraçao finita igual a zero.

== Exemplo:
>>>filtraProjeteisExpirados [projetil1, projetil2, projetil4]
[projetil1, projetil2]

-}

filtraProjeteisExpirados :: [Projetil] -> [Projetil]
filtraProjeteisExpirados projeteis = filter (\p -> duracaoProjetil p /= Finita 0.0 ) projeteis

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
retiraInimigoEmFunçaoDaVida = filter (\inimigo -> vidaInimigo inimigo > 0 )


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

atualizaInimigo :: Inimigo -> Tempo -> Inimigo
atualizaInimigo inimigo tempo =
    ajustaADistancia
        (projeteisalteramVelocidade
          (atualizaMovimentoInimigo inimigo direcoes)
        )
      tempo
  where
    direcoes = criarDirecoes
=======
== Exemplos:
>>> atualizaBase 5 base1
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 150}
>>> atualizaBase 10 base1
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 150}
-}

atualizaBase :: Tempo -> Base -> Base
atualizaBase tempo base =
  base { vidaBase = vidaBase base - somaDanoDosInimigos, creditosBase = creditosBase base + somaButimDosInimigos }
  where
    somaDanoDosInimigos = sum [ataqueInimigo i | i <- inimigosAtuais]
    somaButimDosInimigos = sum [butimInimigo i | i <- inimigosDerrotados]
    inimigosAtuais = filter (atingiuBase base) [inimigo4, inimigo5]  -- Exemplo de inimigos
    inimigosDerrotados = filter (\i -> vidaInimigo i <= 0) inimigosAtuais
>>>>>>> temp

{-|
Verifica se o inimigo atingiu a base.

== Exemplos:
>>> atingiuBase base1 inimigo4
True

>>> atingiuBase base1 inimigo5
False
-}

atingiuBase :: Base -> Inimigo -> Bool
atingiuBase base inimigo = posicaoInimigo inimigo == posicaoBase base

{-|
Atualiza a onda de inimigos no portal, adicionando novos inimigos quando a
entrada da onda é alcançada.

== Exemplos:
>>> atualizaOnda 5 [inimigo4] onda1 
Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0}
             
                                                                                                                                                                                                             
>>> atualizaOnda 6 [inimigo5] onda1
Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0}
-}

atualizaOnda :: Tempo -> [Inimigo] -> Onda -> Onda
atualizaOnda tempo inimigos onda =
  if tempo >= entradaOnda onda
    then let novosInimigos = inimigosOnda onda ++ inimigos
         in onda { inimigosOnda = novosInimigos }
    else onda

{-|
Atualiza os portais, lançando inimigos conforme o ciclo de onda.

<<<<<<< HEAD
== Exemplo: 
>>> atualizaBase base1 6.0 [inimigo4]
Base { posicaoBase = (1, 1), creditosBase = 150, vidaBase = 100.0 }

=======
== Exemplo:
>>> atualizaPortal 5 [inimigo4] portal2
Portal {posicaoPortal = (0.0,3.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0},Onda {inimigosOnda = [Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0}]}
                  
>>>>>>> temp
-}
atualizaPortal :: Tempo -> [Inimigo] -> Portal -> Portal
atualizaPortal tempo inimigos portal =
  portal { ondasPortal = map (atualizaOnda tempo inimigos) (ondasPortal portal) }

{-|
Atualiza as torres no jogo.

== Exemplo:
<<<<<<< HEAD
>>> verificainimigoAtingeBase inimigo2 base
True

=======
>>> atualizaTorre 5 [inimigo4] torre3
Torre {posicaoTorre = (0.0,1.0), danoTorre = 1.0, alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, tempoTorre = -1.0, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}}
>>>>>>> temp
-}
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre tempo inimigos torre = torre { tempoTorre = tempoTorre torre - tempo }

<<<<<<< HEAD
verificainimigoAtingeBase :: Inimigo -> Base -> Bool
verificainimigoAtingeBase inimigo base
   |danoInimigo > 0 = True
   |otherwise = False
    where
        danoInimigo = (vidaBase base - ataqueInimigo inimigo)

{-| A funçao calculaImpactoNaVelocidade calcula a diferença da velociade do inimigo e do efeito do 
projetil que atinge o inimigo. 

== Exemplo:

>>> calculaImpactoNaVelocidade 

-}
calculaImpactoNaVelocidade :: Inimigo -> Projetil -> Float
calculaImpactoNaVelocidade inimigo projetil =
    case tipoProjetil projetil of
        Gelo   -> velocidadeInimigo inimigo 
        Fogo   -> 0  
        Resina -> 2.0 

{-| A funçao mudaAVelocidade aplica a diferença entre velocidade na velocidade original do inimigo, sendo no final 
no maximo 0

==Exemplo
>>> mudaAVelocidade
-}

mudaAVelocidade :: Inimigo -> Inimigo
mudaAVelocidade  inimigo =
    inimigo { velocidadeInimigo = max 0 (velocidadeInimigo inimigo - calculaImpactoNaVelocidade inimigo) }

{-| A funçao ajustaADistancia atualiza a distancia percorrida pelo inimigo 
ajustada em função dos efeitos e do parametro velocidadeInimigo 
=======
{-|
Atualiza os inimigos no jogo: aplica os efeitos dos projéteis (Gelo, Fogo, Resina) e move os inimigos.
>>>>>>> temp

== Exemplo:
>>> atualizaInimigos 5 [torre3] [inimigo4]
[Inimigo {posicaoInimigo = (1.0,-4.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}]
-}
atualizaInimigos :: Tempo -> [Torre] -> [Inimigo] -> [Inimigo]
atualizaInimigos tempo torres inimigos =
  map (\inimigo -> aplicaEfeitoProjetil (inimigo { posicaoInimigo = moverInimigo tempo inimigo })) inimigos
  where
    -- Aplica os efeitos dos projéteis, como Fogo, Gelo e Resina
    aplicarEfeitosProjetil :: Inimigo -> Inimigo
    aplicarEfeitosProjetil inimigo =
      case projeteisInimigo inimigo of
        (Projetil Gelo _:resto) -> inimigo {velocidadeInimigo = 0.0, projeteisInimigo = resto}
        (Projetil Fogo _:resto) -> inimigo {vidaInimigo = vidaInimigo inimigo - 2, projeteisInimigo = resto}
        (Projetil Resina _:resto) -> inimigo {velocidadeInimigo = 0.5, projeteisInimigo = resto}
        _ -> inimigo
{-|
Atualiza um inimigo (move e aplica efeitos de projéteis).

<<<<<<< HEAD
>>> ajustaADistancia inimigo1 3.0
=======
== Exemplo:
>>> atualizaInimigo 5 [torre3] inimigo4
Inimigo {posicaoInimigo = (1.0,-0.25), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
-}
atualizaInimigo :: Tempo -> [Torre] -> Inimigo -> Inimigo
atualizaInimigo tempo torres inimigo =
  let inimigoComEfeitos = aplicaEfeitoProjetil inimigo
  in inimigoComEfeitos { posicaoInimigo = moverInimigo tempo inimigoComEfeitos }
>>>>>>> temp

{-|
Atualiza os projéteis no jogo.

== Exemplo:
>>> atualizaProjetis 5 [projetil4]
[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita (-5.0)}]
-}
atualizaProjetis :: Tempo -> [Projetil] -> [Projetil]
atualizaProjetis tempo projeteis =
  map (atualizaDuracaoProjetil tempo) projeteis

atualizaDuracaoProjetil :: Tempo -> Projetil -> Projetil
atualizaDuracaoProjetil tempo projetil =
  projetil { duracaoProjetil = atualizaDuracao (duracaoProjetil projetil) tempo }

atualizaDuracao :: Duracao -> Tempo -> Duracao
atualizaDuracao Infinita _ = Infinita  -- Se for Infinita, a duração não muda
atualizaDuracao (Finita d) t = Finita (d - t)  -- Se for Finita, subtrai o tempo

{-|
Aplica o efeito de um projétil no inimigo.

== Exemplo:
>>> aplicaEfeito projetil4 inimigo4
Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.5, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
-}
aplicaEfeito :: Projetil -> Inimigo -> Inimigo
aplicaEfeito projetil inimigo =
  case tipoProjetil projetil of
    Fogo  -> aplicaEfeitoFogo projetil inimigo
    Gelo  -> aplicaEfeitoGelo projetil inimigo
    Resina -> aplicaEfeitoResina projetil inimigo

aplicaEfeitoFogo :: Projetil -> Inimigo -> Inimigo
aplicaEfeitoFogo projetil inimigo =
  if tipoProjetil projetil == Fogo then
    inimigo { vidaInimigo = vidaInimigo inimigo - danoPorFogo }
  else
    inimigo
  where
    danoPorFogo = 2  -- Exemplo de dano

aplicaEfeitoGelo :: Projetil -> Inimigo -> Inimigo
aplicaEfeitoGelo projetil inimigo =
  if tipoProjetil projetil == Gelo then
    inimigo { velocidadeInimigo = velocidadeInimigo inimigo * 0.5 }
  else
    inimigo

aplicaEfeitoResina :: Projetil -> Inimigo -> Inimigo
aplicaEfeitoResina projetil inimigo =
  if tipoProjetil projetil == Resina then
    inimigo { velocidadeInimigo = velocidadeInimigo inimigo * 0.8 }
  else
    inimigo

{-|
Move o inimigo com base no tempo e nos efeitos.

== Exemplo:
>>> moverInimigo 5 inimigo4
(1.0,-4.0)
-}
moverInimigo :: Tempo -> Inimigo -> Posicao
moverInimigo tempo inimigo = 
  let (x, y) = posicaoInimigo inimigo
  in case direcaoInimigo inimigo of
       Norte -> (x, y + velocidadeInimigo inimigo * tempo)
       Sul   -> (x, y - velocidadeInimigo inimigo * tempo)
       Este  -> (x + velocidadeInimigo inimigo * tempo, y)
       Oeste -> (x - velocidadeInimigo inimigo * tempo, y)  -- Movimento Oeste

{-|
Calcula a nova posição de um inimigo com base na sua velocidade e direção.

== Exemplo:
>>> calcularNovaPosicao inimigo4 5
(1.0,-4.0)
-}
calcularNovaPosicao :: Inimigo -> Tempo -> Posicao
calcularNovaPosicao inimigo tempo =
  let (x, y) = posicaoInimigo inimigo
      novaPosicao = case direcaoInimigo inimigo of
                      Norte -> (x, y + velocidadeInimigo inimigo * tempo)
                      Sul   -> (x, y - velocidadeInimigo inimigo * tempo)
                      Este  -> (x + velocidadeInimigo inimigo * tempo, y)
                      Oeste -> (x - velocidadeInimigo inimigo * tempo, y)
  in novaPosicao

{-|
Verifica se um inimigo morreu (vida <= 0).

== Exemplo:
>>> inimigoMorreu inimigo4
True
-}
inimigoMorreu :: Inimigo -> Bool
inimigoMorreu inimigo = vidaInimigo inimigo <= 0

{-|
Cria um projetil a partir de uma torre e um inimigo.

== Exemplos:
>>> criarProjetil torre3 inimigo4
Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}

>>> criarProjetil torre3 inimigo5
Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}
-}
ajustaADistancia :: Inimigo -> Tempo  -> Distancia
ajustaADistancia  inimigo velocidadealterada tempo =
    max 0 (velocidadeInimigo (alteraVelocidade inimigo)) * tempo

<<<<<<< HEAD
=======
criarProjetil :: Torre -> Inimigo -> Projetil
criarProjetil torre inimigo = Projetil { tipoProjetil = Fogo , duracaoProjetil = Finita 3.0 }
>>>>>>> temp

{-|
Aplica todos os efeitos de projéteis em um inimigo.

<<<<<<< HEAD
== Exemplo

>>> portalComOndasAtivas portal2
Portal {portal1 = Portal { posicaoPortal = (0, 3), ondasPortal = [onda1]}

-}

portalComOndasAtivas :: Portal -> Portal
portalComOndasAtivas portal = portal { ondasPortal = filter (\onda -> entradaOnda onda <= 0 )  (ondasPortal portal) }

{-| Aplica a portalComOndasAtivas listas de portais.

== Exemplo

>>> portaisComOndasAtivas [portal1, portal2]
[ ]

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
Portal { posicaoPortal = (0, 3), ondasPortal = [Onda = {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 }
,Onda = {inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 } ]}

-}
atualizaPortal :: Tempo -> Portal -> Portal
atualizaPortal tempo portal = portal { ondasPortal = map (atualizaOnda tempo) (ondasPortal portal) }

=======
== Exemplos:
>>> aplicaEfeitoProjetil inimigo4
Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
         
>>> aplicaEfeitoProjetil inimigo5
Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = -2.0, velocidadeInimigo = 0.8, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}
-}

aplicaEfeitoProjetil :: Inimigo -> Inimigo
aplicaEfeitoProjetil inimigo =
  foldl (\acc projetil -> aplicaEfeito projetil acc) inimigo (projeteisInimigo inimigo)
>>>>>>> temp
