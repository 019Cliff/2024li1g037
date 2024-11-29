{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo
-}
module Tarefa2 where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

type Posicao = (Float, Float)

data Terreno = Relva | Agua | Terra
type Mapa = [[Terreno]]

type Creditos = Int
data Base = Base {
vidaBase :: Float,
posicaoBase :: Posicao,
creditosBase :: Creditos
}

type Distancia = Float
type Tempo = Float
data Duracao = Finita Tempo | Infinita
data Torre = Torre {
posicaoTorre :: Posicao,
danoTorre :: Float,
alcanceTorre :: Distancia,
rajadaTorre :: Int,
cicloTorre :: Tempo,
tempoTorre :: Tempo,
projetilTorre :: Projetil
}

data TipoProjetil = Fogo | Gelo | Resina
data Projetil = Projetil {
tipoProjetil :: TipoProjetil,
duracaoProjetil :: Duracao
}



data Direcao = Norte | Sul | Este | Oeste
data Inimigo = Inimigo {
posicaoInimigo :: Posicao,
direcaoInimigo :: Direcao,
vidaInimigo :: Float,
velocidadeInimigo :: Float,
ataqueInimigo :: Float,
butimInimigo :: Creditos,
projeteisInimigo :: [Projetil]
}

data Onda = Onda {
inimigosOnda :: [Inimigo],
cicloOnda :: Tempo,
tempoOnda :: Tempo,
entradaOnda :: Tempo
}

data Portal = Portal {
posicaoPortal :: Posicao,
ondasPortal :: [Onda]
}

type Loja = [(Creditos, Torre)]

data Jogo = Jogo {
baseJogo :: Base,
portaisJogo :: [Portal],
torresJogo :: [Torre],
mapaJogo :: Mapa,
inimigosJogo :: [Inimigo],
lojaJogo :: Loja
}


-- Calcula se os inimigos estão no alcance da torre
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance = undefined
  


--atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo = undefined

--coloca o próximo inimigo a ser lancado por um portal em jogo
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo = undefined


{- A funçao terminouJogo decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
Para isso são utilizadas outras duas funçoes a ganhouJogo e a perdeuJogo.

==Exemplo:
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 15}, inimigosJogo = [] }
True
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 0}, inimigosJogo = = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25}]}
 True
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 15}, inimigosJogo = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25}]}
False 

Nota: No exemplo 1 foi uma vitória, no 2 uma derrota 
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo
 
ganhouJogo :: Jogo -> Bool 
ganhouJogo Jogo {baseJogo = Base {vidaBase = x}, inimigosJogo = inimigos} = x > 0 && null inimigos --nao haver mais inimigos e a torre ter vida assinala a vitória do jogador

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = x}} = x <= 0 --A base náo ter vida assinala a perda do jogo

