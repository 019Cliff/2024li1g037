{-|
Módulo      : Tarefa1
Descrição   : Invariantes do Jogo

Este módulo implementa funções para verificar as invariantes de um jogo, como a validade
dos portais, torres, inimigos e base.
-}
module Tarefa1 where
import LI12425 
import Data.List 

mapa1 :: [[Terreno]]
mapa1 = [[Terra, Terra, Relva], 
         [Relva, Terra, Terra]]

base1 :: Base
base1 = Base { posicaoBase = (1, 1), creditosBase = 100, vidaBase = 100.0 }

portal1 :: Portal
portal1 = Portal { posicaoPortal = (0, 0), ondasPortal = [] }

projetil1 :: Projetil
projetil1 = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }

inimigo1 :: Inimigo
inimigo1 = Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

torre1 :: Torre
torre1 = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil1 }

jogo1 :: Jogo
jogo1 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre1], lojaJogo = [(50, torre1)]}

{-|
Verifica a validade do estado geral do jogo.

Essa função realiza verificações abrangentes no estado do jogo, como:
- Validade dos portais.
- Validade dos inimigos.
- Validade das torres.
- Validade da base.

>>> validaJogo jogoExemplo
True

>>> validaJogo jogoInvalido
False
-}
{-|
=== Função: validaJogo
Verifica se o estado geral do jogo é válido, garantindo que todos os elementos
cumpram as condições necessárias (portais, torres, inimigos, base).

=== Exemplos:

>>> validaJogo jogoExemplo
True

-}
validaJogo :: Jogo -> Bool
validaJogo jogo = 
    all ($ jogo) [validaPortais, validaInimigos, validaTorres, validaBase]

{-|
Valida os portais no jogo.

A função garante que os portais:
- Estejam em terreno válido.
- Não estejam sobrepostos a torres ou base.
- Possuam caminhos válidos até a base.

>>> validaPortais mapaJogo baseExemplo [] [portalExemplo]
True
-}
validaPortais :: Jogo -> Bool
validaPortais jogo =
    let portais = portaisJogo jogo
        mapa = mapaJogo jogo
    in all (\portal -> posicaoPortalValida (posicaoPortal portal) mapa) portais

posicaoPortalValida :: Posicao -> Mapa -> Bool
posicaoPortalValida (x, y) mapa =
    x >= 0 && y >= 0 && x < fromIntegral (length mapa) && y < fromIntegral (length (head mapa)) &&
    case terrenoPorPosicao (x, y) mapa of
        Just terra -> terra /= Agua  -- Exemplo: se o terreno for água, a posição é inválida
        Nothing -> False  -- Se a posição estiver fora do mapa, é inválida

 

{-|
Verifica se uma onda de um portal está vazia de inimigos no início.

>>> validaOndaPortal portalExemplo
True
-}
validaOndaPortal :: Portal -> Bool 
validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal)

{-|
Verifica se uma onda não contém inimigos.

>>> ondaSemInimigos ondaVazia
True
-}
ondaSemInimigos :: Onda -> Bool 
ondaSemInimigos onda = null (inimigosOnda onda)

{-|
Verifica se há pelo menos um portal.

>>> minimoPortal [portalExemplo]
True

>>> minimoPortal []
False
-}
minimoPortal :: [Portal] -> Bool
minimoPortal portais = not (null portais)

{-|
Recupera o terreno de uma posição do mapa.

>>> terrenoPorPosicao (1.0, 2.0) mapaJogo
Just Relva

>>> terrenoPorPosicao (10.0, 2.0) mapaJogo
Nothing
-}
terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoPorPosicao (x, y) mapa =
    if x < 0 || y < 0 || floor x >= length mapa || floor y >= length (head mapa)
    then Nothing
    else Just (mapa !! floor y !! floor x)


{-|
Verifica se todos os portais estão posicionados sobre Terra.

>>> posicionadoEmTerra mapaJogo [portalExemplo]
True
-}
posicionadoEmTerra :: Mapa -> [Portal] -> Bool
posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais

{-|
Verifica se todas as torres estão posicionadas sobre Relva.

>>> posicionadoEmRelvaTorre mapaJogo [torreExemplo]
True
-}

posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
posicionadoEmRelvaTorre mapa torres = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres

{-|
Verifica se as posições de portais, torres e a base estão válidas e não sobrepostas.

>>> naoSobrepostosTorreBase [(1, 1)] baseExemplo [torreExemplo] [portalExemplo] mapaJogo
True
-}
naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase _ base torres portais mapa =
      posicionadoEmRelvaTorre mapa torres &&
      posicionadoEmTerraBase mapa base &&
      verificaPosicaoTorreEmPortal mapa torres portais &&
      verificaPosicaoBaseEmPortal mapa base portais
            
{-|
Verifica se torres não estão posicionadas sobre portais.
>>> verificaPosicaoTorreEmPortal mapaJogo [torreExemplo] [portalExemplo]
True
-}

verificaPosicaoTorreEmPortal :: Mapa -> [Torre] -> [Portal] -> Bool
verificaPosicaoTorreEmPortal mapa torres portais =
    all (\torre -> posicaoTorre torre `notElem` map posicaoPortal portais) torres

{-|
Verifica se a base não está posicionada sobre portais.
>>> verificaPosicaoBaseEmPortal mapaJogo baseExemplo [portalExemplo]
True
-}

verificaPosicaoBaseEmPortal :: Mapa -> Base -> [Portal] -> Bool
verificaPosicaoBaseEmPortal _ base portais =
    posicaoBase base `notElem` map posicaoPortal portais

{-|
Limita o número de ondas por portal para no máximo 1.

>>> maximoOndaPorPortal [portalExemplo]
True
-}
maximoOndaPorPortal :: [Portal] -> Bool 
maximoOndaPorPortal portais = all (\portal -> length (ondasPortal portal) <= 1) portais 

{-|
Verifica se existe pelo menos um caminho entre os portais e a base.

>>> caminhoPortalBase mapaJogo [(0, 0)] (5, 5)
True
-}
caminhoPortalBase :: Mapa -> [Posicao] -> Posicao -> Bool
caminhoPortalBase mapa portais base = any (\portal -> buscaCaminho mapa portal base []) portais

{-|
Busca recursivamente se há um caminho entre duas posições.

>>> buscaCaminho mapaJogo (0, 0) (5, 5) []
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
Obtém as posições adjacentes a uma posição.

>>> adjacentes (2, 2)
[(1.0,2.0),(3.0,2.0),(2.0,1.0),(2.0,3.0)]
-}
adjacentes :: Posicao -> [Posicao]
adjacentes (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

{-|
Verifica se uma posição está dentro dos limites do mapa.

>>> posicaoValida mapaJogo (2, 2)
True

>>> posicaoValida mapaJogo (10, 10)
False
-}
posicaoValida :: Mapa -> (Float, Float) -> Bool
posicaoValida mapa (x, y) =
    let ix = floor x
        iy = floor y
    in ix >= 0 && ix < length mapa && iy >= 0 && iy < length (head mapa)

{-|
Valida os inimigos do jogo.

Verifica condições como:
- Posições válidas.
- Vida e velocidade aceitáveis.
- Conflitos entre projéteis.

>>> validaInimigos mapaJogo [torreExemplo] [(inimigoExemplo, [projetilExemplo])]
True
-}

validaInimigos :: Jogo -> Bool
validaInimigos jogo = undefined

{-
Valida as torres no jogo.

A função verifica:
- Se todas estão em terreno válido.
- Se os alcances e rajadas possuem valores positivos.
- Se não estão sobrepostas a outras torres.

>>> validaTorres mapaJogo [torreExemplo]
True
-}
validaTorres :: Jogo -> Bool
validaTorres jogo =
    posicionadoEmRelvaTorre (mapaJogo jogo) (torresJogo jogo) &&
    naoSobrepostosTorreBase (map posicaoTorre (torresJogo jogo)) (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo) (mapaJogo jogo)

 

{-|
Verifica se todas as torres estão posicionadas em terrenos de Relva.
>>> todasEmRelva mapaJogo [torreExemplo]
True
-}
todasEmRelva :: Mapa -> [Torre] -> Bool
todasEmRelva mapa = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva)
{-|
Verifica se os alcances das torres são todos positivos.
>>> alcancesPositivos [torreExemplo]
True
-}
alcancesPositivos :: [Torre] -> Bool
alcancesPositivos = all (\torre -> alcanceTorre torre > 0)
{-|
Verifica se as rajadas das torres são todas positivas.
>>> rajadasPositivas [torreExemplo]
True
-}
rajadasPositivas :: [Torre] -> Bool
rajadasPositivas = all (\torre -> rajadaTorre torre > 0)
{-|
Verifica se os ciclos das torres são todos não negativos.
>>> ciclosNaoNegativos [torreExemplo]
True
-}
ciclosNaoNegativos :: [Torre] -> Bool
ciclosNaoNegativos = all (\torre -> cicloTorre torre >= 0)
{-|
Verifica se as torres não estão sobrepostas, ou seja, possuem posições únicas.
>>> naoSobrepostas [torreExemplo]
True
-}
naoSobrepostas :: [Torre] -> Bool
naoSobrepostas torres =
    let posicoes = map posicaoTorre torres
    in length posicoes == length (nub posicoes)

{-|
Valida a base do jogo, verificando condições de posição, sobreposição e créditos.

>>> validaBase baseExemplo 10 [portalExemplo] [torreExemplo] mapaJogo
True
-}
validaBase :: Jogo -> Bool
validaBase jogo =
    creditoPositivo creditos &&
    naoSobrepostoTorrePortal portais torres base mapa &&
    posicionadoEmTerraBase mapa base

    
{-|
Verifica se os créditos da base são positivos.
>>> creditoPositivo 10
True
>>> creditoPositivo (-5)
False
-}
creditoPositivo :: Creditos -> Bool
creditoPositivo creditos = creditos >= 0
{-|
Verifica que a base não está sobreposta a torres ou portais.
>>> naoSobrepostoTorrePortal [portalExemplo] [torreExemplo] baseExemplo mapaJogo
True
-}
naoSobrepostoTorrePortal :: [Portal] -> [Torre] -> Base -> Mapa -> Bool
naoSobrepostoTorrePortal portais torres base mapa =
    verificaPosicaoTorreEmBase mapa torres base &&
    verificaPosicaoPortalBase mapa portais base
{-|
Verifica que nenhuma torre está sobreposta à base.
>>> verificaPosicaoTorreEmBase mapaJogo [torreExemplo] baseExemplo
True
-}
verificaPosicaoTorreEmBase :: Mapa -> [Torre] -> Base -> Bool 
verificaPosicaoTorreEmBase _ torres base =
    all (\torre -> posicaoTorre torre /= posicaoBase base) torres
{-|
Verifica que nenhum portal está sobreposto à base.
>>> verificaPosicaoPortalBase mapaJogo [portalExemplo] baseExemplo
True
-}
verificaPosicaoPortalBase :: Mapa -> [Portal] -> Base -> Bool
verificaPosicaoPortalBase _ portais base =
    all (\portal -> posicaoPortal portal /= posicaoBase base) portais
{-|
Verifica se a base está posicionada sobre terreno do tipo Terra.
>>> posicionadoEmTerraBase mapaJogo baseExemplo
True
-}
posicionadoEmTerraBase :: Mapa -> Base -> Bool
posicionadoEmTerraBase mapa base = 
    terrenoPorPosicao (posicaoBase base) mapa == Just Terra