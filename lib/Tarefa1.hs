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


validaJogo :: Jogo -> Bool
validaJogo jogo = 
    all ($ jogo) [validaPortais, validaInimigos, validaTorres, validaBase]


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

 

validaOndaPortal :: Portal -> Bool 
validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal)


ondaSemInimigos :: Onda -> Bool 
ondaSemInimigos onda = null (inimigosOnda onda)


minimoPortal :: [Portal] -> Bool
minimoPortal portais = not (null portais)


terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoPorPosicao (x, y) mapa =
    if x < 0 || y < 0 || floor x >= length mapa || floor y >= length (head mapa)
    then Nothing
    else Just (mapa !! floor y !! floor x)


posicionadoEmTerra :: Mapa -> [Portal] -> Bool
posicionadoEmTerra mapa portais = all (\portal -> terrenoPorPosicao (posicaoPortal portal) mapa == Just Terra) portais


posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
posicionadoEmRelvaTorre mapa torres = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres


naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase _ base torres portais mapa =
      posicionadoEmRelvaTorre mapa torres &&
      posicionadoEmTerraBase mapa base &&
      verificaPosicaoTorreEmPortal mapa torres portais &&
      verificaPosicaoBaseEmPortal mapa base portais

verificaPosicaoTorreEmPortal :: Mapa -> [Torre] -> [Portal] -> Bool
verificaPosicaoTorreEmPortal mapa torres portais =
    all (\torre -> posicaoTorre torre `notElem` map posicaoPortal portais) torres

verificaPosicaoBaseEmPortal :: Mapa -> Base -> [Portal] -> Bool
verificaPosicaoBaseEmPortal _ base portais =
    posicaoBase base `notElem` map posicaoPortal portais


maximoOndaPorPortal :: [Portal] -> Bool 
maximoOndaPorPortal portais = all (\portal -> length (ondasPortal portal) <= 1) portais 


caminhoPortalBase :: Mapa -> [Posicao] -> Posicao -> Bool
caminhoPortalBase mapa portais base = any (\portal -> buscaCaminho mapa portal base []) portais


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


adjacentes :: Posicao -> [Posicao]
adjacentes (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


posicaoValida :: Mapa -> (Float, Float) -> Bool
posicaoValida mapa (x, y) =
    let ix = floor x
        iy = floor y
    in ix >= 0 && ix < length mapa && iy >= 0 && iy < length (head mapa)


validaInimigos :: Jogo -> Bool
validaInimigos jogo = all validaInimigo (inimigosJogo jogo)
  where
    -- Verifica as condições para cada inimigo
    validaInimigo inimigo =
      vidaInimigo inimigo > 0 &&                      -- Vida positiva
      velocidadeInimigo inimigo >= 0 &&              -- Velocidade não negativa
      validaProjeteis (projeteisInimigo inimigo) &&  -- Lista de projéteis normalizada
      not (sobrepoeTorres inimigo (torresJogo jogo)) -- Não sobrepõe torres

    -- Verifica se os projéteis do inimigo estão normalizados
    validaProjeteis projeteis =
      let tipos = map tipoProjetil projeteis
      in length (nub tipos) == length tipos &&       -- Sem projéteis duplicados
         not (incompatíveis tipos)                  -- Sem projéteis incompatíveis

    -- Verifica incompatibilidade de projéteis
    incompatíveis tipos =
      (Fogo `elem` tipos && Resina `elem` tipos) || 
      (Fogo `elem` tipos && Gelo `elem` tipos)

    -- Verifica se o inimigo está sobreposto a uma torre
    sobrepoeTorres inimigo torres =
      any (\torre -> posicaoTorre torre == posicaoInimigo inimigo) torres

validaTorres :: Jogo -> Bool
validaTorres jogo =
    posicionadoEmRelvaTorre (mapaJogo jogo) (torresJogo jogo) &&
    naoSobrepostosTorreBase (map posicaoTorre (torresJogo jogo)) (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo) (mapaJogo jogo)

todasEmRelva :: Mapa -> [Torre] -> Bool
todasEmRelva mapa = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva)

alcancesPositivos :: [Torre] -> Bool
alcancesPositivos = all (\torre -> alcanceTorre torre > 0)

rajadasPositivas :: [Torre] -> Bool
rajadasPositivas = all (\torre -> rajadaTorre torre > 0)

ciclosNaoNegativos :: [Torre] -> Bool
ciclosNaoNegativos = all (\torre -> cicloTorre torre >= 0)

naoSobrepostas :: [Torre] -> Bool
naoSobrepostas torres =
    let posicoes = map posicaoTorre torres
    in length posicoes == length (nub posicoes)


validaBase :: Jogo -> Bool
validaBase jogo =
  baseSobreTerra (mapaJogo jogo) (baseJogo jogo) &&  -- Base está sobre terra
  creditosBase (baseJogo jogo) >= 0 &&              -- Créditos não negativos
  not (sobrepoeTorreOuPortal (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo)) -- Não sobrepõe torre/portal
  where
    -- Verifica se a base está sobre terra no mapa
    baseSobreTerra mapa base =
      case terrenoPorPosicao (posicaoBase base) mapa of
        Just Terra -> True
        _          -> False

    -- Verifica se a base sobrepõe alguma torre ou portal
    sobrepoeTorreOuPortal base torres portais =
      let posBase = posicaoBase base
      in any (\torre -> posicaoTorre torre == posBase) torres ||
         any (\portal -> posicaoPortal portal == posBase) portais

posicionadoEmTerraBase :: Mapa -> Base -> Bool
posicionadoEmTerraBase mapa base = 
    terrenoPorPosicao (posicaoBase base) mapa == Just Terra
