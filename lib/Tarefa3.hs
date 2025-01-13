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


torre3 :: Torre
torre3 = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil2 }

projetil4 :: Projetil
projetil4 = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 0.0 }

inimigo4 :: Inimigo
inimigo4 = Inimigo { posicaoInimigo = (1, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (4, 1), direcaoInimigo = Sul, velocidadeInimigo = 1.0, vidaInimigo = 0.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}

onda1 :: Onda
onda1 = Onda { inimigosOnda = [inimigo4], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0 }

portal2 :: Portal
portal2 = Portal { posicaoPortal = (0, 3), ondasPortal = [onda1, onda2] }

onda2 :: Onda
onda2 = Onda { inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0 }

jogo2 :: Jogo
jogo2 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre3], lojaJogo = [(50, torre3)]}

{-|
Atualiza o estado do jogo, incluindo os inimigos, torres, portais e a base.

== Exemplos:
>>> atualizaJogo 5 [torre3] [inimigo4] portal2 base1
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 200}

>>> atualizaJogo 10 [torre3] [inimigo4, inimigo5] portal2 base1
<<<<<<< HEAD
Base {vidaBase = 100.0, posicaoBase = (1.0,1.0), creditosBase = 200}
=======
Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 250}

>>>>>>> temp
-}

atualizaJogo :: Tempo -> [Torre] -> [Inimigo] -> Portal -> Base -> Base
atualizaJogo tempo torres inimigos portal base =
  let torresAtualizadas = map (\t -> atualizaTorre tempo inimigos t) torres
      inimigosAtualizados = atualizaInimigos tempo mapa1 inimigos
      portalAtualizado = atualizaPortal tempo inimigos portal
      baseAtualizada = atualizaBase tempo (base {creditosBase = creditosBase base + butim})
      butim = sum [butimInimigo i | i <- inimigosDerrotados]
      inimigosDerrotados = filter (\i -> vidaInimigo i <= 0) inimigos
  in baseAtualizada

{-|
Atualiza a base: perde vida se inimigos atingem ela.

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
    inimigosAtuais = filter (atingiuBase base) [inimigo4, inimigo5]  
    inimigosDerrotados = filter (\i -> vidaInimigo i <= 0) inimigosAtuais

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

== Exemplo:
>>> atualizaPortal 5 [inimigo4] portal2
Portal {posicaoPortal = (0.0,3.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0},Onda {inimigosOnda = [Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0}]}
                  
-}

atualizaPortal :: Tempo -> [Inimigo] -> Portal -> Portal
atualizaPortal tempo inimigos portal =
  portal { ondasPortal = map (atualizaOnda tempo inimigos) (ondasPortal portal) }

{-|
Atualiza as torres no jogo.

== Exemplo:
>>> atualizaTorre 5 [inimigo4] torre3
Torre {posicaoTorre = (0.0,1.0), danoTorre = 1.0, alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, tempoTorre = -1.0, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}}
-}

atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre tempo inimigos torre = torre { tempoTorre = tempoTorre torre - tempo }

{-|
Atualiza os inimigos no jogo: aplica os efeitos dos projéteis (Gelo, Fogo, Resina) e move os inimigos.

== Exemplo:
>>> atualizaInimigos 5 mapa1 [inimigo4]
[Inimigo {posicaoInimigo = (1.0,2.25), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}]
-}

atualizaInimigos :: Tempo -> Mapa -> [Inimigo] -> [Inimigo]
atualizaInimigos tempo mapa inimigos = map (atualizaInimigo tempo mapa) inimigos


{-|
Atualiza o estado de um inimigo no jogo, aplicando os efeitos dos projéteis que o atingiram e atualizando a sua posição.

== Exemplo:
>>> atualizaInimigo 5 mapa1 inimigo4
Inimigo {posicaoInimigo = (1.0,2.25), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
             
-}

atualizaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
atualizaInimigo tempo mapa inimigo =
  let inimigoComEfeitos = aplicaEfeitoProjetil inimigo
      inimigoMovido = moverInimigo tempo mapa inimigoComEfeitos
  in inimigoComEfeitos { posicaoInimigo = posicaoInimigo inimigoMovido }

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
atualizaDuracao Infinita _ = Infinita  
atualizaDuracao (Finita d) t = Finita (d - t)  

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
    danoPorFogo = 2 

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
Determina a próxima direção válida para um inimigo, com base no mapa e na direção atual.
A direção é determinada pela posição atual do inimigo e as direções possíveis (Norte, Sul, Este, Oeste).

== Exemplo:
>>> proximaDirecao mapa1 (1, 1) Sul
Norte

-}

proximaDirecao :: Mapa -> Posicao -> Direcao -> Direcao
proximaDirecao mapa (x, y) direcaoAtual =
  let (ix, iy) = (floor x, floor y) 
      vizinhos = [(Norte, (ix, iy - 1)), (Sul, (ix, iy + 1)), (Este, (ix + 1, iy)), (Oeste, (ix - 1, iy))]
      terrenoValido (_, (cx, cy)) =
          cx >= 0 && cy >= 0 && 
          cy < length mapa && 
          cx < length (head mapa) && 
          mapa !! cy !! cx == Terra
      direcoesValidas = filter terrenoValido vizinhos
  in case direcoesValidas of
       [] -> direcaoAtual  
       (novaDirecao, _):_ -> novaDirecao

{-|
Atualiza a posição do inimigo com base na direção.
A função determina a nova direção utilizando a função 'proximaDirecao' e atualiza a posição do inimigo, considerando sua velocidade e o tempo decorrido.

== Exemplo:
>>> moverInimigo 1.0 mapa1 inimigo4
Inimigo {posicaoInimigo = (1.0,2.0), direcaoInimigo = Norte, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
-}

moverInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
moverInimigo tempo mapa inimigo =
  let novaDirecao = proximaDirecao mapa (posicaoInimigo inimigo) (direcaoInimigo inimigo)
      (x, y) = posicaoInimigo inimigo
      novoPos = case novaDirecao of
        Norte -> (x, y + velocidadeInimigo inimigo * tempo)
        Sul   -> (x, y - velocidadeInimigo inimigo * tempo)
        Este  -> (x + velocidadeInimigo inimigo * tempo, y)
        Oeste -> (x - velocidadeInimigo inimigo * tempo, y)
  in inimigo { posicaoInimigo = novoPos, direcaoInimigo = novaDirecao }

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

criarProjetil :: Torre -> Inimigo -> Projetil
criarProjetil torre inimigo = Projetil { tipoProjetil = Fogo , duracaoProjetil = Finita 3.0 }

{-|
Aplica todos os efeitos de projéteis em um inimigo.

== Exemplos:
>>> aplicaEfeitoProjetil inimigo4
Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}
         
>>> aplicaEfeitoProjetil inimigo5
Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = -2.0, velocidadeInimigo = 0.8, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}
-}

aplicaEfeitoProjetil :: Inimigo -> Inimigo
aplicaEfeitoProjetil inimigo =
  foldl (\acc projetil -> aplicaEfeito projetil acc) inimigo (projeteisInimigo inimigo)
