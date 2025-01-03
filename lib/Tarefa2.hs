{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo.
-}
module Tarefa2 where

import LI12425

{-| A função inimigosNoAlcance calcula os inimigos ao alcance de uma dada torre.

== Exemplo:
 >>> inimigosNoAlcance Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5.0)} [] 
 []
 >>> inimigosNoAlcance Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5.0)}  [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}]
 [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}]
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance Torre {posicaoTorre = (x, y), alcanceTorre = alcance} inimigos =
  filter (\Inimigo {posicaoInimigo = (a, b)} -> sqrt ((x - a) ** 2 + (y - b) ** 2) <= alcance) inimigos

{-| A função atingeInimigo atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre.
A vida do inimigo diminui tanto quanto o dano que o projetil da torre e, conforme o tipo de projetil, atualiza a lista de projeteis ativos de acordo com as sinergias.

== Exemplo:
>>> atingeInimigo Torre {posicaoTorre = (0, 0), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5.0)} Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}
Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 90.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]}
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetilNovo} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis} =
  inimigo {
    vidaInimigo = max 0 (vida - dano),
    projeteisInimigo = projetilNovo : projeteis
  }

{-| A função fogoEGelo caso tenha projeteis de Fogo e Gelo ativos retira-os da lista.

== Exemplo:
>>> fogoEGelo [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 2.0}]
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
>>> encontraGelo [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
>>> encontraGelo []
[]
-}
encontraGelo :: [Projetil] -> [Projetil]
encontraGelo = filter (\p -> tipoProjetil p == Gelo)

{-| A função atingeFogoEResina caso tenha projeteis de Fogo e Resina ativos retira os de Resina e dobra a duração do de Fogo.

== Exemplo:
>>> atingeFogoEResina [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}]
>>> atingeFogoEResina [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
-}
atingeFogoEResina :: [Projetil] -> [Projetil]  
atingeFogoEResina projeteis =
      if not (null (filter (\p -> tipoProjetil p == Fogo) projeteis)) && not (null (filter (\p -> tipoProjetil p == Resina) projeteis))
      then let fogo = Projetil Fogo (duplicaDuracao (duracaoProjetil (head (filter (\p -> tipoProjetil p == Fogo) projeteis))))
           in [fogo]
      else projeteis

{-| Devolve uma lista com apenas projeteis do tipo Fogo presentes na lista.

== Exemplo:
>>> encontraFogo [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]
>>> encontraFogo []
[]
-}
encontraFogo :: [Projetil] -> [Projetil]
encontraFogo = filter (\p -> tipoProjetil p == Fogo)

{-| Devolve uma lista com apenas projeteis do tipo Resina presentes na lista.

== Exemplo:
>>> encontraResina [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
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
duplicaDuracao (Finita t1) = Finita (t1 * 2)
duplicaDuracao Infinita = Infinita

{-| Divide uma lista de projeteis em três listas diferentes de acordo com o tipo de projetil.

== Exemplo:
>>> dividePorTipoProjetil [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 1.0}]
([Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}],[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 1.0}])
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
>>> somaDuracoes [Finita 5.0, Finita 10.0]
Finita 15.0
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
>>> verificaIguais [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]
True
>>> verificaIguais [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
False
-}
verificaIguais :: [Projetil] -> Bool
verificaIguais projeteis =
  let (fogos, gelos, resinas) = dividePorTipoProjetil projeteis
  in length fogos > 1 || length gelos > 1 || length resinas > 1

{-| Ativa o próximo inimigo a ser lançado por um portal.

== Exemplo:
>>> ativaInimigo Portal {posicaoPortal = (0.0, 0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}], cicloOnda = 0.0, tempoOnda = 0.0, entradaOnda = 0.0}]} []
(Portal {posicaoPortal = (0.0,0.5), ondasPortal = []},[Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}])
>>> ativaInimigo Portal {posicaoPortal = (0.0, 0.5), ondasPortal = []} []
(Portal {posicaoPortal = (0.0,0.5), ondasPortal = []},[])
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
>>> terminouJogo Jogo {baseJogo = Base {vidaBase = 15, posicaoBase = (0,0), creditosBase = 100}, inimigosJogo = [], portaisJogo = [], torresJogo = [], mapaJogo = [], lojaJogo = []}
True

>>> terminouJogo Jogo {baseJogo = Base {vidaBase = 0, posicaoBase = (0,0), creditosBase = 100}, inimigosJogo = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25.0, direcaoInimigo = Norte, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}], portaisJogo = [], torresJogo = [], mapaJogo = [], lojaJogo = []}
True

>>> terminouJogo Jogo {baseJogo = Base {vidaBase = 15, posicaoBase = (0,0), creditosBase = 100}, inimigosJogo = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25.0, direcaoInimigo = Norte, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = []}], portaisJogo = [], torresJogo = [], mapaJogo = [], lojaJogo = []}
False

Nota: No exemplo 1 foi uma vitória, no exemplo 2 foi uma derrota.
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo
ganhouJogo :: Jogo -> Bool 
ganhouJogo Jogo {baseJogo = Base {vidaBase = x}, inimigosJogo = inimigos} = x > 0 && null inimigos

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = x}} = x <= 0


