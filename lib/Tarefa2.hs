{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo
-}
module Tarefa2 where

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

{-| A funçao inimigosNoAlcance calcula os inimigos ao alcance de uma dada torre.

==Exemplo:
 >>>inimigosNoAlcance torre@Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2} [] 
 []

>>>inimigosNoAlcance torre@Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2}  [inimigo@Inimigo {posicaoInimigo = (0.5,0.5)}]
inimigosNoAlcance torre@Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2} [inimigo@Inimigo {posicaoInimigo = (0.5,0.5)}]


>>>inimigosNoAlcance torre@Torre {posicaoTorre = (1.5,1.5), alcanceTorre = 2}  [inimigo@Inimigo {posicaoInimigo = (0.5,0.5),inimigo@Inimigo {posicaoInimigo = (4.5,4.5)}, inimigo@Inimigo { posicaoInimigo = (1.5,1.5)}]
 [inimigo@Inimigo {posicaoInimigo = (0.5, 0.5)}, inimigo@Inimigo {posicaoInimigo = (1.5, 1.5)}]


-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre@Torre {posicaoTorre = (a,b), alcanceTorre = x} [] = []
inimigosNoAlcance torre@Torre {posicaoTorre = (a,b), alcanceTorre = x} (inimigo@Inimigo {posicaoInimigo = (x1,y1)} :is)
      |sqrt ((x1 - a) ^ 2 + (y1 - b) ^ 2) <= x = inimigo : inimigosNoAlcance torre is
      | otherwise = inimigosNoAlcance torre is
  


--atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo = undefined

{-| A funçao ativaInimigo e coloca o próximo inimigo a ser lançado por um portal em jogo

==Exemplo:

>>>ativaInimigo Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [inimigo3, inimigo4, inimigo5]} : os)} [inimigo1, inimigo2]
(Portal {posicaoPortal = (0, 0.5), ondasPortal = onda {inimigosOnda = [inimigo4, inimigo5]} : os}, [inimigo1, inimigo2, inimigo3])

>>>ativaInimigo Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [] } : os)} inimigosjogo
(Portal {posicaoPortal = (0, 0.5), ondasPortal = onda {inimigosOnda = is} : os}, inimigosjogo)
 
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo Portal {posicaoPortal = (x,y), ondasPortal = (onda@Onda {inimigosOnda = (i:is)} : os)} inimigosjogo = 
             (Portal {posicaoPortal = (x,y), ondasPortal = onda {inimigosOnda = is}: os}, inimigosjogo  ++ [i])      
ativaInimigo Portal {posicaoPortal = (x,y), ondasPortal = (onda@Onda {inimigosOnda = []} : os)} inimigosjogo =
             ( Portal {posicaoPortal = (x,y), ondasPortal = onda {inimigosOnda = []} : os}, inimigosjogo) 
ativaInimigo portal inimigosjogo = (portal, inimigosjogo) 


{-| A funçao terminouJogo decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
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

