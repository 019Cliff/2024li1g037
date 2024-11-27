{-|
Módulo      : Tarefa1
Descrição   : Invariantes do Jogo

Este módulo implementa funções para verificar as invariantes de um jogo, como a validade
dos portais, torres, inimigos e base.
-}
module Tarefa1 where
import LI12425
import Data.List

{-|
Um exemplo de mapa para o jogo.
Cada posição é representada como sendo de Terra, Relva ou Água.

>>> mapaJogo !! 0
[Terra, Terra, Relva, Agua, Agua, Agua]

>>> mapaJogo !! 1
[Relva, Terra, Relva, Agua, Relva, Relva]
-}
mapaJogo :: Mapa
mapaJogo = [[t, t, r, a, a, a],
            [r, t, r, a, r, r],
            [r, t, r, a, r, t],
            [r, t, r, a, r, t],
            [r, t, t, t, t, t],
            [a, a, a, a, r, r]
            ]
      where
            t = Terra
            r = Relva
            a = Agua

{-|
Verifica se o estado geral do jogo é válido.

A função `validaJogo` realiza uma série de verificações no estado atual do jogo, validando diferentes aspectos, como:
- A validade dos portais (existência mínima, posicionamento em Terra, caminho até a base, entre outros).
- A validade dos inimigos, incluindo suas posições, vida, velocidade e interação com torres.
- A validade das torres, verificando se estão posicionadas corretamente, com alcances e rajadas positivos e sem sobreposição.
- A validade da base, garantindo que esteja sobre um terreno válido (Terra), que não esteja sobreposta a outras entidades (torres ou portais) e que os créditos sejam suficientes.

>>> validaJogo jogoExemplo
True

>>> validaJogo jogoInvalido
False
-}
validaJogo :: Jogo -> Bool
validaJogo jogo = 
    validaPortais mapa base torres portais &&
    validaInimigos mapa torres inimigosProjeteis &&
    validaTorres mapa torres && 
    validaBase base creditos portais torres mapa

{-|
Verifica se todos os portais do jogo cumprem as condições impostas.

>>> validaPortais mapaJogo baseExemplo [] [portalExemplo]
True
-}
validaPortais :: Mapa -> Base -> [Torre] -> [Portal] -> Bool
validaPortais mapa base torres portais =
    minimoPortal portais && 
    posicionadoEmTerra mapa portais &&
    caminhoPortalBase mapa (map posicaoPortal portais) (posicaoBase base) && 
    naoSobrepostosTorreBase (map posicaoPortal portais) base torres portais mapa && 
    maximoOndaPorPortal portais 

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
    let linha = mapa !! (floor y)
        terreno = linha !! (floor x)
    in Just terreno

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
{-|
Verifica se as posições de portais, torres e a base estão válidas e não sobrepostas.

>>> naoSobrepostosTorreBase [(1, 1)] baseExemplo [torreExemplo] [portalExemplo] mapaJogo
True
-}
naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase posicoes base torres portais mapa =
      posicionadoEmRelvaTorre mapa torres &&
      posicionadoEmTerraBase mapa base &&
      verificaPosicaoTorreEmPortal mapa torres portais &&
      verificaPosicaoBaseEmPortal mapa base portais
            where
                  {-|
                  Verifica se a base está posicionada sobre Terra.

                  >>> posicionadoEmTerraBase mapaJogo baseExemplo
                  True
                  -}
                  posicionadoEmTerraBase :: Mapa -> Base -> Bool
                  posicionadoEmTerraBase mapa base =
                        terrenoPorPosicao (posicaoBase base) mapa == Just Terra

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
                  verificaPosicaoBaseEmPortal mapa base portais =
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
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (x, y) =
    floor x >= 0 && floor x < length mapa && floor y >= 0 && floor y < length (head mapa)

{-|
Valida os inimigos do jogo de acordo com as regras.

>>> validaInimigos mapaJogo [torreExemplo] [(inimigoExemplo, [projetilExemplo])]
True
-}
validaInimigos :: Mapa -> [Torre] -> [(Inimigo, [Projetil])] -> Bool
validaInimigos mapa torres inimigosProjeteis = 
    all validaInimigo inimigosProjeteis
    where
        {-|
        Verifica se um único inimigo é válido.

        >>> validaInimigo (inimigoExemplo, [projetilExemplo])
        True
        -}
        validaInimigo :: (Inimigo, [Projetil]) -> Bool
        validaInimigo (inimigo, projeteis) =
            posicaoSobreTerra (posicaoInimigo inimigo) &&
            vidaInimigo inimigo > 0 &&
            velocidadeInimigo inimigo >= 0 &&
            listaSemDuplicatas projeteis &&
            naoConflitantes projeteis &&
            not (sobrepostoComTorre inimigo torres)

        {-|
        Verifica se uma posição está sobre Terra.

        >>> posicaoSobreTerra (2, 2)
        True
        -}
        posicaoSobreTerra :: Posicao -> Bool
        posicaoSobreTerra pos = terrenoPorPosicao pos mapa == Just Terra

        {-|
        Verifica se uma lista de projéteis não contém duplicatas.

        >>> listaSemDuplicatas [projetilExemplo]
        True
        -}
        listaSemDuplicatas :: [Projetil] -> Bool
        listaSemDuplicatas projeteis =
            let tipos = map tipoProjetil projeteis
            in length tipos == length (nub tipos)

        {-|
        Verifica que projéteis não têm conflitos (e.g., Fogo com Gelo).

        >>> naoConflitantes [projetilExemplo]
        True
        -}
        naoConflitantes :: [Projetil] -> Bool
        naoConflitantes projeteis =
            let tipos = map tipoProjetil projeteis
            in not (Fogo `elem` tipos && Resina `elem` tipos) &&
               not (Fogo `elem` tipos && Gelo `elem` tipos)

        {-|
        Verifica se um inimigo não está sobreposto a torres.

        >>> sobrepostoComTorre inimigoExemplo [torreExemplo]
        False
        -}
        sobrepostoComTorre :: Inimigo -> [Torre] -> Bool
        sobrepostoComTorre inimigo = any (\torre -> posicaoInimigo inimigo == posicaoTorre torre)

{-|
Valida as torres do jogo, verificando condições como posição, alcance, rajadas, ciclos, e sobreposição.

>>> validaTorres mapaJogo [torreExemplo]
True
-}
validaTorres :: Mapa -> [Torre] -> Bool
validaTorres mapa torres =
    todasEmRelva mapa torres && 
    alcancesPositivos torres &&
    rajadasPositivas torres &&  
    ciclosNaoNegativos torres && 
    naoSobrepostas torres 

    where
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
validaBase :: Base -> Creditos -> [Portal] -> [Torre] -> Mapa -> Bool
validaBase base creditos portais torres mapa =
    creditoPositivo creditos &&
    naoSobrepostoTorrePortal portais torres base mapa &&
    posicionadoEmTerraBase mapa base

    where
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
        verificaPosicaoTorreEmBase mapa torres base =
            all (\torre -> posicaoTorre torre /= posicaoBase base) torres

        {-|
        Verifica que nenhum portal está sobreposto à base.

        >>> verificaPosicaoPortalBase mapaJogo [portalExemplo] baseExemplo
        True
        -}
        verificaPosicaoPortalBase :: Mapa -> [Portal] -> Base -> Bool
        verificaPosicaoPortalBase mapa portais base =
            all (\portal -> posicaoPortal portal /= posicaoBase base) portais

        {-|
        Verifica se a base está posicionada sobre terreno do tipo Terra.

        >>> posicionadoEmTerraBase mapaJogo baseExemplo
        True
        -}
        posicionadoEmTerraBase :: Mapa -> Base -> Bool
        posicionadoEmTerraBase mapa base = 
            terrenoPorPosicao (posicaoBase base) mapa == Just Terra
