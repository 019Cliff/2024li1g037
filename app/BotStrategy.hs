module BotStrategy
  ( StrategicAction (..),
    StrategicDecision (..),
    decideBotJogo,
    applyStrategicAction,
  )
where

import Data.Function (on)
import Data.List (sortBy)
import EnemySystem
import LI12425
import MapGeometry (terrenoEmCelula)
import MetaTypes
import TowerSystem

data StrategicAction
  = StrategicBuild TowerId Posicao
  | StrategicUpgrade Posicao
  | StrategicSave
  | StrategicNone
  deriving (Show, Read, Eq)

data StrategicDecision = StrategicDecision
  { strategicAction :: StrategicAction,
    strategicScore :: Float,
    strategicReason :: String,
    strategicAlternatives :: [(StrategicAction, Float)]
  }
  deriving (Show, Read, Eq)

decideBotJogo :: Jogo -> StrategicDecision
decideBotJogo jogoAtual =
  let ameacas = classesVisiveis jogoAtual
      candidatas = construcoes jogoAtual ameacas ++ upgrades jogoAtual
      ordenadas = sortBy (flip (compare `on` snd)) candidatas
      fallback = (StrategicSave, 0.05)
      todas = ordenadas ++ [fallback]
      (acao, score) = case todas of
        [] -> (StrategicNone, 0)
        primeira : _ -> primeira
   in StrategicDecision acao score (explica acao ameacas) (take 5 todas)

applyStrategicAction :: Jogo -> StrategicAction -> Maybe (Jogo, String)
applyStrategicAction jogoAtual acao = case acao of
  StrategicBuild towerId pos -> do
    entrada <- findEntry towerId (lojaJogoEstrategica jogoAtual)
    let preco = shopPrice entrada
        torre = shopTower entrada
    let base = baseJogo jogoAtual
    if creditosBase base < preco || not (legal pos) then Nothing else
      let torreNova = torre {posicaoTorre = pos}
          jogoNovo = jogoAtual {baseJogo = base {creditosBase = creditosBase base - preco}, torresJogo = torreNova : torresJogo jogoAtual}
       in Just (jogoNovo, "Bot estrategico: construcao")
  StrategicUpgrade pos -> do
    torre <- findFirst ((== pos) . posicaoTorre) (torresJogo jogoAtual)
    let custo = custoUpgradeTorre torre
        base = baseJogo jogoAtual
    if creditosBase base < custo then Nothing else
      let torreNova = upgradeTorre torre
          jogoNovo = jogoAtual {baseJogo = base {creditosBase = creditosBase base - custo}, torresJogo = substituir torre torreNova (torresJogo jogoAtual)}
       in Just (jogoNovo, "Bot estrategico: upgrade")
  StrategicSave -> Nothing
  StrategicNone -> Nothing
  where
    legal pos = let (x, y) = (floor (fst pos), floor (snd pos)) in terrenoEmCelula (mapaJogo jogoAtual) x y == Just Relva && all ((/= pos) . posicaoTorre) (torresJogo jogoAtual)
    findEntry towerId = findFirst ((== towerId) . shopTowerId)
    findFirst _ [] = Nothing
    findFirst predicado (x : xs) = if predicado x then Just x else findFirst predicado xs
    substituir alvo nova = map (\torre -> if posicaoTorre torre == posicaoTorre alvo then nova else torre)

classesVisiveis :: Jogo -> [(EnemyClass, Int)]
classesVisiveis jogoAtual =
  let ativas = map enemyClassOf (inimigosJogo jogoAtual)
      preview = case concatMap ondasPortal (portaisJogo jogoAtual) of
        onda : _ -> map enemyClassOf (inimigosOnda onda)
        [] -> []
      todas = ativas ++ preview
   in [(classe, length (filter (== classe) todas)) | classe <- [minBound .. maxBound], classe `elem` todas]

construcoes :: Jogo -> [(EnemyClass, Int)] -> [(StrategicAction, Float)]
construcoes jogoAtual ameacas =
  [ (StrategicBuild (shopTowerId entrada) pos, valor entrada pos)
  | entrada <- lojaJogoEstrategica jogoAtual,
    shopPrice entrada <= creditosBase (baseJogo jogoAtual),
    pos <- melhoresPosicoes jogoAtual (shopTower entrada),
    let valor entradaAtual posicao =
          let torre = shopTower entradaAtual
              mapa = mapaJogo jogoAtual
              coberturaRota = cobertura mapa torre posicao
              valorCombate = resposta (shopTowerId entradaAtual) ameacas
              valorEmergencia = emergencia (baseJogo jogoAtual) mapa torre posicao
              desperdicio = alcanceDesperdicado mapa torre posicao
           in 0.55 * coberturaRota
                + 0.25 * valorCombate
                + 0.12 * valorEmergencia
                - 0.08 * desperdicio
                + 0.23 * (coberturaRota + valorCombate) / fromIntegral (max 1 (shopPrice entradaAtual))
  ]

melhoresPosicoes :: Jogo -> Torre -> [Posicao]
melhoresPosicoes jogoAtual torre =
  take 8
    (sortBy
      (flip (compare `on` \pos -> cobertura (mapaJogo jogoAtual) torre pos))
      (posicoesLegais jogoAtual))

lojaJogoEstrategica :: Jogo -> [ShopEntry]
lojaJogoEstrategica jogoAtual =
  [ ShopEntry towerId preco torre
  | (preco, torre) <- lojaJogo jogoAtual,
    let towerId = towerIdSpec (towerSpecAproximada torre)
  ]

upgrades :: Jogo -> [(StrategicAction, Float)]
upgrades jogoAtual =
  [ (StrategicUpgrade (posicaoTorre torre), danoTorre torre / fromIntegral (max 1 (custoUpgradeTorre torre)))
  | torre <- torresJogo jogoAtual,
    custoUpgradeTorre torre <= creditosBase (baseJogo jogoAtual)
  ]

posicoesLegais :: Jogo -> [Posicao]
posicoesLegais jogoAtual =
  [ (fromIntegral x + 0.5, fromIntegral y + 0.5)
  | (y, linha) <- zip [0 :: Int ..] (mapaJogo jogoAtual),
    (x, terreno) <- zip [0 :: Int ..] linha,
    terreno == Relva,
    let pos = (fromIntegral x + 0.5, fromIntegral y + 0.5),
    all ((/= pos) . posicaoTorre) (torresJogo jogoAtual),
    pertoDaRota x y (mapaJogo jogoAtual)
  ]

pertoDaRota :: Int -> Int -> Mapa -> Bool
pertoDaRota x y mapa = any rota [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    rota (cx, cy) = terrenoEmCelula mapa cx cy `elem` [Just Terra, Just Asfalto]

cobertura :: Mapa -> Torre -> Posicao -> Float
cobertura mapa torre pos =
  let rota = celulasRota mapa
      cobertas = length [() | ponto <- rota, distancia pos ponto <= alcanceTorre torre]
   in min 1 (fromIntegral cobertas / fromIntegral (max 1 (length rota)))

celulasRota :: Mapa -> [Posicao]
celulasRota mapa =
  [ (fromIntegral x + 0.5, fromIntegral y + 0.5)
  | (y, linha) <- zip [0 :: Int ..] mapa,
    (x, terreno) <- zip [0 :: Int ..] linha,
    terreno `elem` [Terra, Asfalto]
  ]

emergencia :: Base -> Mapa -> Torre -> Posicao -> Float
emergencia base mapa torre pos =
  let alvo = posicaoBase base
      perto = distancia pos alvo
   in if perto <= alcanceTorre torre * 1.5 && cobertura mapa torre pos > 0.05 then 1 else 0

alcanceDesperdicado :: Mapa -> Torre -> Posicao -> Float
alcanceDesperdicado mapa torre pos =
  let rota = celulasRota mapa
      distanciaMinima = minimum (map (distancia pos) rota)
   in min 1 (max 0 (alcanceTorre torre - distanciaMinima) / max 1 (alcanceTorre torre))

resposta :: TowerId -> [(EnemyClass, Int)] -> Float
resposta towerId ameacas =
  let tags = tagsTowerSpec (towerSpec towerId)
      enxame = sum [n | (classe, n) <- ameacas, Enxame `elem` tagsEnemySpec (enemySpec classe)]
      armadura = sum [n | (classe, n) <- ameacas, Armadura `elem` tagsEnemySpec (enemySpec classe)]
   in min 1
        (0.2
          + (if Area `elem` tags || Chain `elem` tags then fromIntegral enxame / 10 else 0)
          + (if Burst `elem` tags then fromIntegral armadura / 12 else 0)
          + (if Slow `elem` tags || Fear `elem` tags then fromIntegral enxame / 14 else 0))

distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x1 - x2
    dy = y1 - y2

explica :: StrategicAction -> [(EnemyClass, Int)] -> String
explica acao _ = case acao of
  StrategicBuild towerId pos -> "Construir " ++ nomeTowerSpec (towerSpec towerId) ++ " em " ++ show pos ++ " para cobrir a rota"
  StrategicUpgrade pos -> "Melhorar torre em " ++ show pos ++ " pelo melhor ganho por credito"
  StrategicSave -> "Poupar creditos para uma resposta melhor"
  StrategicNone -> "Nenhuma acao legal"
