{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo.
-}
module Tarefa2 where

import LI12425

base1 :: Base
base1 = Base { posicaoBase = (1, 1), creditosBase = 100, vidaBase = 100.0 }

portal1 :: Portal
portal1 = Portal { posicaoPortal = (0, 0), ondasPortal = [] }

projetil1 :: Projetil
projetil1 = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }

projetil2 :: Projetil
projetil2 = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }

projetil3 :: Projetil
projetil3 = Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0 }

inimigo1 :: Inimigo
inimigo1 = Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

torre1 :: Torre
torre1 = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil1 }

jogo1 :: Jogo
jogo1 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre1], lojaJogo = [(50, torre1)]}

{-| A função inimigosNoAlcance calcula os inimigos ao alcance de uma dada torre.

== Exemplo:
 >>> inimigosNoAlcance torre1 []
 []
 >>> inimigosNoAlcance torre1 [inimigo1]
 [inimigo1]
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance Torre {posicaoTorre = (x, y), alcanceTorre = alcance} inimigos =
  filter (\Inimigo {posicaoInimigo = (a, b)} -> sqrt ((x - a) ** 2 + (y - b) ** 2) <= alcance) inimigos

{-| A função atingeInimigo atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre.
A vida do inimigo diminui tanto quanto o dano que o projetil da torre causa, e o projetil é adicionado à lista de projeteis ativos do inimigo.

== Exemplo:
>>> atingeInimigo torre1 inimigo1
inimigo1 {vidaInimigo = 99.0, projeteisInimigo = [projetil1]}
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetilNovo} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis} =
  inimigo {
    vidaInimigo = max 0 (vida - dano),
    projeteisInimigo = projetilNovo : projeteis
  }

{-| A função fogoEGelo remove projeteis de Fogo e Gelo ativos simultaneamente.

== Exemplo:
>>> fogoEGelo [projetil1, projetil2, projetil3]
[projetil3]
>>> fogoEGelo []
[]
-}
fogoEGelo :: [Projetil] -> [Projetil]  
fogoEGelo projeteis =
    if not (null (filter (\p -> tipoProjetil p == Fogo) projeteis)) && not (null (filter (\p -> tipoProjetil p == Gelo) projeteis))
    then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) projeteis
    else projeteis

{-| Devolve uma lista com apenas projeteis do tipo Gelo presentes na lista.

== Exemplo:
>>> encontraGelo [projetil2, projetil3, projetil2]
[projetil2, projetil2]
>>> encontraGelo []
[]
-}
encontraGelo :: [Projetil] -> [Projetil]
encontraGelo = filter (\p -> tipoProjetil p == Gelo)

{-| A função atingeFogoEResina remove projeteis de Resina e dobra a duração de projeteis de Fogo quando ambos estão ativos.

== Exemplo:
>>> atingeFogoEResina [projetil1, projetil3, projetil2]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}]
>>> atingeFogoEResina [projetil2]
[projetil2]
-}
atingeFogoEResina :: [Projetil] -> [Projetil]  
atingeFogoEResina projeteis =
      if not (null (filter (\p -> tipoProjetil p == Fogo) projeteis)) && not (null (filter (\p -> tipoProjetil p == Resina) projeteis))
      then let fogo = Projetil Fogo (duplicaDuracao (duracaoProjetil (head (filter (\p -> tipoProjetil p == Fogo) projeteis))))
           in [fogo]
      else projeteis

{-| Devolve uma lista com apenas projeteis do tipo Fogo presentes na lista.

== Exemplo:
>>> encontraFogo [projetil1, projetil2, projetil1]
[projetil1, projetil1]
>>> encontraFogo []
[]
-}
encontraFogo :: [Projetil] -> [Projetil]
encontraFogo = filter (\p -> tipoProjetil p == Fogo)

{-| Devolve uma lista com apenas projeteis do tipo Resina presentes na lista.

== Exemplo:
>>> encontraResina [projetil3, projetil2, projetil3]
[projetil3, projetil3]
>>> encontraResina []
[]
-}
encontraResina :: [Projetil] -> [Projetil]
encontraResina = filter (\p -> tipoProjetil p == Resina)

{-| Dobra a duração de um projetil.

== Exemplo:
>>> duplicaDuracao (Finita 5.0)
Finita 10.0
>>> duplicaDuracao Infinita
Infinita
-}
duplicaDuracao :: Duracao -> Duracao
duplicaDuracao (Finita t) = Finita (2 * t)
duplicaDuracao Infinita   = Infinita

{-| Divide uma lista de projeteis em três listas diferentes de acordo com o tipo de projetil.

== Exemplo:
>>> dividePorTipoProjetil [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 1.0}]
([Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}],[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 1.0}])
>>> dividePorTipoProjetil []
([],[],[])
-}

{-| Divide uma lista de projeteis em três listas diferentes de acordo com o tipo de projetil.

== Exemplo:
>>> dividePorTipoProjetil [projetil1, projetil2, projetil3]
([Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}],
 [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}],
 [Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}])
>>> dividePorTipoProjetil []
([],[],[])
-}
dividePorTipoProjetil :: [Projetil] -> ([Projetil], [Projetil], [Projetil])
dividePorTipoProjetil projeteis = (fogos, gelos, resinas)
      where
       fogos = filter (\p -> tipoProjetil p == Fogo) projeteis
       gelos = filter (\p -> tipoProjetil p == Gelo) projeteis
       resinas = filter (\p -> tipoProjetil p == Resina) projeteis

{-| Soma as durações de uma lista de projeteis.

== Exemplo:
>>> somaDuracoes [Finita 5.0, Finita 3.0, Finita 4.0]
Finita 12.0
>>> somaDuracoes [Finita 5.0, Infinita]
Infinita
-}
somaDuracoes :: [Duracao] -> Duracao
somaDuracoes [] = Finita 0
somaDuracoes (Infinita : _) = Infinita
somaDuracoes (Finita t : ds) = case somaDuracoes ds of
  Finita restante -> Finita (t + restante)
  Infinita -> Infinita

{-| Verifica se existem projeteis repetidos de tipo igual.

== Exemplo:
>>> verificaIguais [projetil1, projetil2, projetil1]
True
>>> verificaIguais [projetil2, projetil3]
False
-}
verificaIguais :: [Projetil] -> Bool
verificaIguais projeteis =
  let (fogos, gelos, resinas) = dividePorTipoProjetil projeteis
  in length fogos > 1 || length gelos > 1 || length resinas > 1

{-| Ativa o próximo inimigo a ser lançado por um portal.

== Exemplo:
>>> ativaInimigo portal1 []
(Portal {posicaoPortal = (0,0), ondasPortal = []},
 [Inimigo {posicaoInimigo = (0,0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}])
>>> ativaInimigo portal1 []
(Portal {posicaoPortal = (0,0), ondasPortal = []},[])
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal@Portal {ondasPortal = []} inimigos = (portal, inimigos)
ativaInimigo portal@Portal {ondasPortal = (onda@Onda {inimigosOnda = []} : outrasOndas)} inimigos =
  (portal {ondasPortal = outrasOndas}, inimigos)
ativaInimigo portal@Portal {ondasPortal = (onda@Onda {inimigosOnda = (i:is), cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada} : outrasOndas)} inimigos =
  let novaOnda = Onda {inimigosOnda = is, cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada}
      novasOndas = if null is then outrasOndas else (novaOnda : outrasOndas)
  in (portal {ondasPortal = novasOndas}, inimigos ++ [i])

{-| A função terminouJogo decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
Para isso são utilizadas outras duas funções a ganhouJogo e a perdeuJogo.

== Exemplo:
>>> terminouJogo jogo1
False
>>> terminouJogo jogo1 {baseJogo = base1 {vidaBase = 0}}
True

Nota: No exemplo 1 o jogo continua, no exemplo 2 foi uma derrota.
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo
ganhouJogo :: Jogo -> Bool 
ganhouJogo Jogo {baseJogo = Base {vidaBase = x}, inimigosJogo = inimigos} = x > 0 && null inimigos

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = x}} = x <= 0

