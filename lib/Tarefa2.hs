{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo.
-}
module Tarefa2 where

import LI12425

{-|
A função `inimigosNoAlcance` calcula os inimigos ao alcance de uma dada torre.

== Exemplo:
>>> inimigosNoAlcance Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5)} []
[]
>>> inimigosNoAlcance Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5)} [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 100, projeteisInimigo = []}]
[Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 100.0, projeteisInimigo = []}]
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance Torre {posicaoTorre = (x, y), alcanceTorre = alcance} inimigos =
  filter (\Inimigo {posicaoInimigo = (a, b)} -> sqrt ((x - a) ** 2 + (y - b) ** 2) <= alcance) inimigos

{-|
A função `atingeInimigo` calcula o dano de uma torre em um inimigo, atualizando a vida do inimigo e adicionando um projétil de fogo.

== Exemplo:
>>> atingeInimigo Torre {posicaoTorre = (0, 0), alcanceTorre = 2, danoTorre = 10, rajadaTorre = 1, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 5)} Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 100, projeteisInimigo = []}
Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 90.0, projeteisInimigo = [Projetil Fogo (Finita 5)]}
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = Projetil tipo duracao} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis} =
    let novaVida = max 0 (vida - dano)
        novosProjeteis = Projetil tipo duracao : projeteis
    in inimigo {vidaInimigo = novaVida, projeteisInimigo = novosProjeteis}

{-|
A função `somaDuracao` soma as durações de dois projéteis.

== Exemplo:
>>> somaDuracao (Finita 3) (Finita 5)
Finita 8.0
>>> somaDuracao Infinita (Finita 5)
Infinita
-}
somaDuracao :: Duracao -> Duracao -> Duracao
somaDuracao (Finita t1) (Finita t2) = Finita (t1 + t2)
somaDuracao Infinita _ = Infinita
somaDuracao _ Infinita = Infinita

{-|
A função `fogoEGelo` remove os projéteis de Fogo e Gelo da lista caso ambos estejam ativos.

== Exemplo:
>>> fogoEGelo [Projetil Fogo (Finita 5), Projetil Gelo (Finita 3)]
[]
-}
fogoEGelo :: [Projetil] -> [Projetil]
fogoEGelo projeteis =
    if not (null (encontraTipo Fogo projeteis)) && not (null (encontraTipo Gelo projeteis))
    then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) projeteis
    else projeteis

encontraTipo :: TipoProjetil -> [Projetil] -> [Projetil]
encontraTipo tipo = filter ((== tipo) . tipoProjetil)

{-|
A função `atingeFogoEResina` retira projéteis de Resina e dobra a duração dos projéteis de Fogo, caso ambos estejam ativos.

== Exemplo:
>>> atingeFogoEResina [Projetil Fogo (Finita 5), Projetil Resina (Finita 2)]
[Projetil Fogo (Finita 10)]
-}
atingeFogoEResina :: [Projetil] -> [Projetil]
atingeFogoEResina projeteis =
    if not (null (encontraTipo Fogo projeteis)) && not (null (encontraTipo Resina projeteis))
    then map (\p -> if tipoProjetil p == Fogo then Projetil Fogo (dobroDuracao (duracaoProjetil p)) else p)
         (filter (\p -> tipoProjetil p /= Resina) projeteis)
    else projeteis

{-|
Dobra a duração de um projétil.
-}
dobroDuracao :: Duracao -> Duracao
dobroDuracao (Finita t) = Finita (t * 2)
dobroDuracao Infinita = Infinita

{-|
A função `ativaInimigo` coloca o próximo inimigo a ser lançado por um portal em jogo.

== Exemplo:
>>> ativaInimigo Portal {posicaoPortal = (0, 0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0,0), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 100, projeteisInimigo = []}, Inimigo {posicaoInimigo = (1,1), direcaoInimigo = Sul, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 15, vidaInimigo = 80, projeteisInimigo = []}]}]} []
(Portal {posicaoPortal = (0, 0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1,1), direcaoInimigo = Sul, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 15, vidaInimigo = 80, projeteisInimigo = []}]}]}, [Inimigo {posicaoInimigo = (0,0), direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 10, vidaInimigo = 100, projeteisInimigo = []}])
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal@Portal {ondasPortal = []} inimigos = (portal, inimigos)
ativaInimigo portal@Portal {ondasPortal = (Onda {inimigosOnda = []} : outrasOndas)} inimigos =
    (portal {ondasPortal = outrasOndas}, inimigos)
ativaInimigo portal@Portal {ondasPortal = (Onda {inimigosOnda = (i:is), cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada} : outrasOndas)} inimigos =
    let novaOnda = Onda {inimigosOnda = is, cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada}
        novoPortal = portal {ondasPortal = novaOnda : outrasOndas}
    in (novoPortal, i : inimigos)


-- Ajustando os valores padrão para os campos adicionais de Onda
novaOndaPadrao :: [Inimigo] -> Onda
novaOndaPadrao inimigos = Onda {inimigosOnda = inimigos, cicloOnda = 0, tempoOnda = 0, entradaOnda = 0}

-- Função auxiliar para corrigir formatação de números
formatarInimigo :: Inimigo -> Inimigo
formatarInimigo inimigo@Inimigo {vidaInimigo = vida, velocidadeInimigo = vel, ataqueInimigo = atk} = 
    inimigo 
        { vidaInimigo = fromIntegral (round (vida :: Float) :: Int) :: Float
        , velocidadeInimigo = fromIntegral (round (vel :: Float) :: Int) :: Float
        , ataqueInimigo = fromIntegral (round (atk :: Float) :: Int) :: Float
        }

{-|
A função `terminouJogo` decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

ganhouJogo :: Jogo -> Bool
ganhouJogo Jogo {baseJogo = Base {vidaBase = vida}, inimigosJogo = inimigos} = vida > 0 && null inimigos

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = vida}} = vida <= 0
