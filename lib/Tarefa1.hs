{-|
Módulo      : Tarefa1
Descrição   : Invariantes do Jogo
Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
              Ines Braga da Silva <a112819@alunos.uminho.pt>

Este módulo implementa funções para verificar as invariantes de um jogo, como a validade
dos portais, torres, inimigos e base.
-}
module Tarefa1 where
import LI12425 
import Data.List 

mapa1 :: [[Terreno]]
mapa1 = [
    [t, t, t, t, t, t,t, t, t, t, t, t, t,t, t, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r ,r,r ,r, r, r, t, r, r, r, r, r, r, r, a, a ,a, r, r, r, r, r , r,r , r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, t, t, t, t, t,t, t, t, t, t, t, t,t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t]
    ]
    where
        t = Terra
        r = Relva
        a = Agua

base1 :: Base
base1 = Base { posicaoBase = (-20.5,13), creditosBase = 100, vidaBase = 100.0 }

portal1 :: Portal
portal1 = Portal { posicaoPortal = (15, 8.5), ondasPortal = [] }

inimigo1 :: Inimigo
inimigo1 = Inimigo { posicaoInimigo = (-20, 10), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

torre1 :: Torre
torre1 = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0 }

jogo1 :: Jogo
jogo1 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre1], lojaJogo = [(50, torre1)]}


{-|
Verifica se o estado de um jogo é válido.

Esta função avalia a validade dos componentes do jogo, como portais, torres, inimigos e a base.

=== Exemplo de utilização:
>>> validaJogo jogo1
True
-}
validaJogo :: Jogo -> Bool
validaJogo jogo = 
    all ($ jogo) [validaPortais, validaInimigos, validaTorres, validaBase]

{-|
Verifica se todos os portais de um jogo estão em posições válidas no mapa.

=== Exemplo de utilização:
>>> validaPortais jogo1
True
-}
validaPortais :: Jogo -> Bool
validaPortais jogo =
    let portais = portaisJogo jogo
        mapa = mapaJogo jogo
    in all (\portal -> posicaoPortalValida (posicaoPortal portal) mapa) portais

{-|
Verifica se uma posição de portal é válida no mapa.

Uma posição é válida se estiver dentro dos limites do mapa e o terreno correspondente não for "Água".

=== Exemplo de utilização:
>>> posicaoPortalValida (0, 0) mapa1
True
-}
posicaoPortalValida :: Posicao -> Mapa -> Bool
posicaoPortalValida (x, y) mapa =
    x >= 0 && y >= 0 && x < fromIntegral (length mapa) && y < fromIntegral (length (head mapa)) &&
    case terrenoPorPosicao (x, y) mapa of
        Just terra -> terra /= Agua
        Nothing -> False

{-|
Verifica se uma onda de inimigos associada a um portal é válida.

Uma onda é válida se não possuir inimigos.

=== Exemplo de utilização:
>>> validaOndaPortal portal1
True
-}
validaOndaPortal :: Portal -> Bool 
validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal)

{-|
Verifica se uma onda está vazia (sem inimigos).

=== Exemplo de utilização:
>>> ondaSemInimigos (Onda [] 1 1 1)
True
-}

ondaSemInimigos :: Onda -> Bool 
ondaSemInimigos onda = null (inimigosOnda onda)

{-|
Verifica se existe pelo menos um portal no jogo.

=== Exemplo de utilização:
>>> minimoPortal [portal1]
True
-}
minimoPortal :: [Portal] -> Bool
minimoPortal portais = not (null portais)

{-|
Obtém o terreno correspondente a uma posição no mapa.

Retorna `Nothing` se a posição estiver fora dos limites do mapa.

=== Exemplo de utilização:
>>> terrenoPorPosicao (0, 0) mapa1
Just Terra
-}
terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoPorPosicao (x, y) mapa =
    if x < 0 || y < 0 || floor x >= length mapa || floor y >= length (head mapa)
    then Nothing
    else Just (mapa !! floor y !! floor x)

{-|
Verifica se todos os portais estão posicionados sobre terrenos "Terra" no mapa.

=== Exemplo de utilização:
>>> posicionadoEmTerra mapa1 [portal1]
True
-}
posicionadoEmTerra :: Mapa -> [Portal] -> Bool
posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais

{-|
Verifica se todas as torres estão posicionadas sobre terrenos "Relva" no mapa.

=== Exemplo de utilização:
>>> posicionadoEmRelvaTorre mapa1 [torre1]
True
-}
posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
posicionadoEmRelvaTorre mapa torres = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres

{-|
Verifica se torres, portais e a base não estão sobrepostos.

=== Exemplo de utilização:
>>> naoSobrepostosTorreBase [] base1 [torre1] [portal1] mapa1
True
-}
naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase _ base torres portais mapa =
      posicionadoEmRelvaTorre mapa torres &&
      posicionadoEmTerraBase mapa base &&
      verificaPosicaoTorreEmPortal mapa torres portais &&
      verificaPosicaoBaseEmPortal mapa base portais

{-|
Verifica se uma torre não está sobre um portal.

=== Exemplo de utilização:
>>> verificaPosicaoTorreEmPortal mapa1 [torre1] [portal1]
True
-}
verificaPosicaoTorreEmPortal :: Mapa -> [Torre] -> [Portal] -> Bool
verificaPosicaoTorreEmPortal _ torres portais =
    all (\torre -> posicaoTorre torre `notElem` map posicaoPortal portais) torres

{-|
Verifica se a base não está sobre um portal.

=== Exemplo de utilização:
>>> verificaPosicaoBaseEmPortal mapa1 base1 [portal1]
True
-}
verificaPosicaoBaseEmPortal :: Mapa -> Base -> [Portal] -> Bool
verificaPosicaoBaseEmPortal _ base portais =
    posicaoBase base `notElem` map posicaoPortal portais

{-|
Verifica se há no máximo uma onda por portal.

=== Exemplo de utilização:
>>> maximoOndaPorPortal [portal1]
True
-}
maximoOndaPorPortal :: [Portal] -> Bool 
maximoOndaPorPortal portais = all (\portal -> length (ondasPortal portal) <= 1) portais

{-|
Verifica se existe um caminho entre qualquer portal e a base no mapa.

=== Exemplo de utilização:
>>> caminhoPortalBase mapa1 [(1, 1), (2, 2)] (0, 0)
True
-}
caminhoPortalBase :: Mapa -> [Posicao] -> Posicao -> Bool
caminhoPortalBase mapa portais base = any (\portal -> buscaCaminho mapa portal base []) portais

{-|
Realiza a busca de um caminho entre uma posição atual e o destino no mapa.

A busca considera a validade da posição, terrenos e evita visitar posições repetidas.

=== Exemplo de utilização:
>>> buscaCaminho mapa1 (1, 1) (0, 0) []
True
-}
buscaCaminho :: Mapa -> Posicao -> Posicao -> [Posicao] -> Bool
buscaCaminho mapa atual destino visitados
    | atual == destino = True
    | not (posicaoValida mapa atual) = False
    | atual `elem` visitados = False
    | terrenoPorPosicao atual mapa /= Just Terra = False
    | otherwise =
        let visitados' = atual : visitados
            vizinhos = adjacentes atual
        in any (\pos -> buscaCaminho mapa pos destino visitados') vizinhos

{-|
Obtém as posições adjacentes a uma posição dada.

=== Exemplo de utilização:
>>> adjacentes (1.0, 1.0)
[(0.0,1.0),(2.0,1.0),(1.0,0.0),(1.0,2.0)]
-}
adjacentes :: Posicao -> [Posicao]
adjacentes (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

{-|
Verifica se uma posição é válida no mapa, considerando os limites.

=== Exemplo de utilização:
>>> posicaoValida mapa1 (1.5, 2.5)
True
-}
posicaoValida :: Mapa -> (Float, Float) -> Bool
posicaoValida mapa (x, y) =
    let ix = floor x
        iy = floor y
    in ix >= 0 && ix < length mapa && iy >= 0 && iy < length (head mapa)

{-|
Verifica as condições de validade dos inimigos no jogo.

=== Exemplo de utilização:
>>> validaInimigos jogo1
True
-}

validaInimigos :: Jogo -> Bool
validaInimigos jogo = all validaInimigo (inimigosJogo jogo)
  where
    -- Verifica as condições para cada inimigo
    validaInimigo inimigo =
      vidaInimigo inimigo > 0 &&                     
      velocidadeInimigo inimigo >= 0 &&              
      validaProjeteis (projeteisInimigo inimigo) &&  
      not (sobrepoeTorres inimigo (torresJogo jogo)) 

    -- Verifica se os projéteis do inimigo estão normalizados
    validaProjeteis projeteis =
      let tipos = map tipoProjetil projeteis
      in length (nub tipos) == length tipos &&      
         not (incompatíveis tipos)                  

    -- Verifica incompatibilidade de projéteis
    incompatíveis tipos =
      (Fogo `elem` tipos && Resina `elem` tipos) || 
      (Fogo `elem` tipos && Gelo `elem` tipos)

    -- Verifica se o inimigo está sobreposto a uma torre
    sobrepoeTorres inimigo torres =
      any (\torre -> posicaoTorre torre == posicaoInimigo inimigo) torres

{-|
Função que valida se todas as torres no jogo estão corretamente posicionadas e não têm conflitos.

Verifica se todas as torres estão posicionadas sobre o terreno correto (relva) e se não há sobreposição de torres com a base, portais ou outras torres.

=== Exemplo de utilização:
>>> validaTorres jogo1
True
-}
validaTorres :: Jogo -> Bool
validaTorres jogo =
    posicionadoEmRelvaTorre (mapaJogo jogo) (torresJogo jogo) &&
    naoSobrepostosTorreBase (map posicaoTorre (torresJogo jogo)) (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo) (mapaJogo jogo)

{-|
Função que verifica se todas as torres estão posicionadas sobre o terreno de relva.

Verifica se cada torre está sobre uma célula de relva no mapa.

=== Exemplo de utilização:
>>> todasEmRelva (mapaJogo jogo1) (torresJogo jogo1)
True
-}
todasEmRelva :: Mapa -> [Torre] -> Bool
todasEmRelva mapa = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva)

{-|
Função que verifica se todas as torres têm um alcance positivo.

Verifica se o alcance de cada torre é maior que 0.

=== Exemplo de utilização:
>>> alcancesPositivos (torresJogo jogo1)
True
-}
alcancesPositivos :: [Torre] -> Bool
alcancesPositivos = all (\torre -> alcanceTorre torre > 0)

{-|
Função que verifica se todas as torres têm rajadas de tiro positivas.

Verifica se o número máximo de inimigos que uma torre pode atingir em uma rajada de tiro é maior que 0.

=== Exemplo de utilização:
>>> rajadasPositivas (torresJogo jogo1)
True
-}
rajadasPositivas :: [Torre] -> Bool
rajadasPositivas = all (\torre -> rajadaTorre torre > 0)

{-|
Função que verifica se todas as torres têm ciclos de rajada não negativos.

Verifica se o tempo entre as rajadas de cada torre é maior ou igual a 0.

=== Exemplo de utilização:
>>> ciclosNaoNegativos (torresJogo jogo1)
True
-}
ciclosNaoNegativos :: [Torre] -> Bool
ciclosNaoNegativos = all (\torre -> cicloTorre torre >= 0)

{-|
Função que verifica se não há sobreposição de torres.

Verifica se as torres não estão sobrepondo umas às outras.

=== Exemplo de utilização:
>>> naoSobrepostas (torresJogo jogo1)
True
-}
naoSobrepostas :: [Torre] -> Bool
naoSobrepostas torres =
    let posicoes = map posicaoTorre torres
    in length posicoes == length (nub posicoes)

{-|
Função que valida o estado da base do jogo.

Verifica se a base está posicionada sobre um terreno de terra, se o número de créditos é não negativo e se a base não sobrepõe nenhuma torre ou portal.

=== Exemplo de utilização:
>>> validaBase jogo1
True
-}
validaBase :: Jogo -> Bool
validaBase jogo =
  baseSobreTerra (mapaJogo jogo) (baseJogo jogo) &&  
  creditosBase (baseJogo jogo) >= 0 &&              
  not (sobrepoeTorreOuPortal (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo))
  where
    
    baseSobreTerra mapa base =
      case terrenoPorPosicao (posicaoBase base) mapa of
        Just Terra -> True
        _          -> False

    
    sobrepoeTorreOuPortal base torres portais =
      let posBase = posicaoBase base
      in any (\torre -> posicaoTorre torre == posBase) torres ||
         any (\portal -> posicaoPortal portal == posBase) portais

{-|
Função que verifica se a base está posicionada sobre um terreno de terra.

Verifica se o terreno onde a base está posicionada no mapa é do tipo `Terra`.

=== Exemplo de utilização:
>>> posicionadoEmTerraBase (mapaJogo jogo1) (baseJogo jogo1)
True
-}
posicionadoEmTerraBase :: Mapa -> Base -> Bool
posicionadoEmTerraBase mapa base = 
    terrenoPorPosicao (posicaoBase base) mapa == Just Terra
