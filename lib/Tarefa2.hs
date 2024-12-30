{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo
-}
module Tarefa2 where

import LI12425

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
  


{-| A funçao atingeInimigo atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre, 
a vida do inimigo diminiu tanto quanto o dano que o projetil da torre e consoante o tipo de projetil, atualiza a lista de projeteis ativos de 
acordo com as sinergias. 

==Exemplo 

>>> atingeInimigo 


-}



atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo =
      inimigo {
          vidaInimigo = vidaInimigo inimigo - danoTorre torre,
          projeteisInimigo = novosProjeteis (projeteisInimigo inimigo)
    }
  where
    novosProjeteis :: [Projetil]  -> [Projetil]
    novosProjeteis projeteis
        | not (null (encontraFogo projeteis)) && not (null (encontraGelo projeteis)) = fogoEGelo projeteis 
        | not (null (encontraFogo projeteis)) && not (null (encontraResina projeteis)) = atingeFogoEResina projeteis 
        | verificaIguais projeteis = projeteisIguais projeteis 
        | otherwise = atualizaLista projeteis
        

{-| A funçao lida com o caso de o inimigo nao ser atingido por projeteis não resultam em sinergias.

==Exemplo
>>>atualizaLista [Resina] Gelo
[Resina, Gelo]

-}


atualizaLista :: [Projetil] -> Projetil -> [Projetil]  
atualizaLista [] projetilnovo = [projetilnovo]
atualizaLista projeteis projetilnovo =  projeteis ++ [projetilnovo]


{-|A função fogoEGelo caso tenha projeteis de Fogo e Gelo ativos retira-os da lista. 


==Exemplo:

>>>fogoEGelo [Fogo, Resina, Gelo] 
[Resina]

>>> fogoEGelo []
[]
-}

fogoEGelo ::  [Projetil] -> [Projetil]  
fogoEGelo projeteis =
    if not (null (encontraFogo projeteis)) && not (null (encontraGelo projeteis))
    then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) projeteis
    else projeteis


{-| Devolve uma lista com apenas Gelo está na lista de projeteis 

==Exemplo:

>>> encontraGelo [Gelo, Resina, Gelo, Gelo, Fogo, Fogo] 
[Gelo, Gelo, Gelo]

>>> encontreGelo []
[]

-}
encontraGelo :: [Projetil] -> [Projetil]
encontraGelo [] = []
encontraGelo (x:xs) 
                    | tipoProjetil x == Gelo = x : encontraGelo xs
                    | otherwise = encontraGelo xs

      

{-| A função atingeFogoEResina caso tenha projeteis de Fogo e Resina ativos retira os de Resina e dobra a duração do de Fogo.


==Exemplo:

>>> atingeFogoEResina [Gelo, Resina, Gelo, Gelo, Fogo] 
[Fogo]

>>> atingeFogoEResina [Gelo, Resina, Gelo]
[Gelo, Resina, Gelo]

-}


atingeFogoEResina ::  [Projetil] -> [Projetil]  
atingeFogoEResina projeteis =
      if not (null (encontraFogo projeteis)) && not (null (encontraResina projeteis))  
      then Projetil Fogo novaDuracao : filter (\x -> tipoProjetil x /= Resina) projeteis
      else projeteis 
       where
    novaDuracao = duplicaDuracao (duracaoProjetil (head (encontraFogo projeteis)))
  

{-| Devolve uma lista com apenas fogo está na lista de projeteis  

==Exemplo:

>>> encontraFogo [Gelo, Resina, Gelo, Gelo, Fogo, Fogo] 
[Fogo, Fogo]

>>> encontraFogo []
[]

-} 
encontraFogo :: [Projetil] -> [Projetil]
encontraFogo [] = []
encontraFogo (x:xs) 
                    | tipoProjetil x == Fogo = x : encontraFogo xs
                    | otherwise = encontraFogo xs

{-| Devolve uma lista com apenas resina na lista de projeteis  

==Exemplo:

>>> encontraResina [Gelo, Resina, Gelo, Gelo, Fogo, Fogo] 
[Resina]

>>> encontraResina []
[]

-} 


encontraResina :: [Projetil] -> [Projetil]
encontraResina [] = []
encontraResina (x:xs) 
                    | tipoProjetil x == Resina = x : encontraResina xs
                    | otherwise = encontraResina xs


{-|Dobra a duraçao de um projetil

== Exemplo

>>> duplicaDuracao (Finita 5.0)
Finita 10.0

>>>duplicaDuracao Infinita
Infinita
-}
duplicaDuracao :: Duracao -> Duracao
duplicaDuracao (Finita t1) = Finita (t1 * 2)
duplicaDuracao Infinita = Infinita




{-| A funçao divide uma lista de projeteis em 3 lista diferentes de cada tipo de projetil.

==Exemplo


>>> dividePorTipoProjetil [Fogo, Gelo, Fogo, Fogo, Resina, Fogo, Gelo, Resina]
([Fogo,Fogo,Fogo,Fogo],[Gelo,Gelo],[Resina,Resina])

-}
dividePorTipoProjetil :: [Projetil] -> ([Projetil], [Projetil], [Projetil])
dividePorTipoProjetil projeteis = (fogos, gelos, resinas)
      where
       fogos = filter (\p -> tipoProjetil p == Fogo ) projeteis
       gelos = filter (\p -> tipoProjetil p == Gelo) projeteis
       resinas = filter (\p -> tipoProjetil p == Resina ) projeteis



{-| A funçao soma as duraçoes de listas com tipo de projetil iguais.

==Exemplo
 >>> somaDuracaoDeListaProjeteisIguais [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }]
Finita 10.0
 
>>> somaDuracaoDeListaProjeteisIguais [Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }]
Finita 8.0

>>> somaDuracaoDeListaProjeteisIguais [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }]
Finita 9.0

-}
somaDuracaoDeListaProjeteisIguais :: [Projetil]  -> Duracao
somaDuracaoDeListaProjeteisIguais projeteis 
     | verificaIguais projeteis = foldl somaDuracao (Finita 0.0) (extraiDuracoes projeteis)
     | otherwise = Finita 0.0

{-|A funçao extraí as durações de uma lista de projéteis.

==Exemplos

>>> extraiDuracoes [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }]
[Finita 5.0, Finita 4.0]

>>> extraiDuracoes [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Resina, duracaoProjetil = Infinito }]
[Finita 5.0, Infinito]

-}
extraiDuracoes :: [Projetil] -> [Duracao]
extraiDuracoes [] = []
extraiDuracoes (p:ps) = duracaoProjetil p : extraiDuracoes ps

{-| A funçao soma duraçoes.

==Exemplo
>>> somaDuracao (Finita 5.0) (Finita 20.0)
Finita 25.0

>>> somaDuracao (Finita 10.0) Infinita
Infinita
-}
somaDuracao :: Duracao -> Duracao -> Duracao
somaDuracao (Finita t1) (Finita t2) = Finita (t1 + t2)
somaDuracao Infinita  _ = Infinita
somaDuracao _ Infinita = Infinita



{-| Verifica se todos os projeteis de uma lista são iguais.

>>>verificaIguais [Fogo, Gelo, Fogo]
False
>>> verificaIguais [Gelo, Gelo, Gelo]
True

-}
verificaIguais :: [Projetil] -> Bool
verificaIguais [] = False
verificaIguais [_] = True 
verificaIguais (x:xs) = all (\p -> tipoProjetil p == tipoProjetil x) xs




{-| Funçao que face a uma lista de projeteis do mesmo tipo, soma as suas duraçoes e deixa apenas um projetil por tipo  

>>> projeteisIguais [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}]


>>> projeteisIguais [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 8.0 }]


projeteisIguais [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = 3 }, Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]


-}
projeteisIguais :: [Projetil] -> [Projetil]
projeteisIguais projeteis =  concat [
    [Projetil Fogo (somaDuracaoDeListaProjeteisIguais fogos)],
    [Projetil Gelo (somaDuracaoDeListaProjeteisIguais gelos)],
    [Projetil Resina (somaDuracaoDeListaProjeteisIguais resinas)]
  ]
  where
    (fogos, gelos, resinas) = dividePorTipoProjetil projeteis

{-| A funçao ativaInimigo coloca o próximo inimigo a ser lançado por um portal em jogo.


>>>ativaInimigo Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [inimigo3, inimigo4, inimigo5]} : os)} [inimigo1, inimigo2]    
(Portal {posicaoPortal = (0, 0.5), ondasPortal = []}, [inimigo1,inimigo2])


>>>ativaInimigo Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [] } : os)} inimigosjogo
(Portal {posicaoPortal = (0, 0.5), ondasPortal = onda {inimigosOnda = is} : os}, inimigosjogo)

>>> ativaInimigo Portal {posicaoPortal = (x, y), ondasPortal = []} [inimigo1,inimigo2]
             (Portal {posicaoPortal = (x, y), ondasPortal = []}, [inimigo1,inimigo2])
 
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo Portal {posicaoPortal = (x, y), ondasPortal = []} inimigosjogo =
             (Portal {posicaoPortal = (x, y), ondasPortal = []}, inimigosjogo)
ativaInimigo Portal {posicaoPortal = (x,y), ondasPortal = (onda@Onda {inimigosOnda = []} : os)} inimigosjogo =
             ( Portal {posicaoPortal = (x,y), ondasPortal = onda {inimigosOnda = []} : os}, inimigosjogo) 
ativaInimigo Portal {posicaoPortal = (x,y), ondasPortal = (onda@Onda {inimigosOnda = (i:is)} : os)} inimigosjogo = 
             (Portal {posicaoPortal = (x,y), ondasPortal = onda {inimigosOnda = is}: os}, inimigosjogo  ++ [i])      



{-| A funçao terminouJogo decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
Para isso são utilizadas outras duas funçoes a ganhouJogo e a perdeuJogo.

==Exemplo:
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 15}, inimigosJogo = [] }
True
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 0}, inimigosJogo = = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25}]}
 True
>>>terminouJogo Jogo {baseJogo = Base {vidaBase = 15}, inimigosJogo = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25}]}
False 

Nota: No exemplo 1 foi uma vitória, no exemplo 2 foi uma derrota 
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo
 
ganhouJogo :: Jogo -> Bool 
ganhouJogo Jogo {baseJogo = Base {vidaBase = x}, inimigosJogo = inimigos} = x > 0 && null inimigos --nao haver mais inimigos e a torre ter vida assinala a vitória do jogador

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = x}} = x <= 0 --A base náo ter vida assinala a perda do jogo

