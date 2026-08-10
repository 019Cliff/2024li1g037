{-# LANGUAGE OverloadedStrings #-}

module CatalogExport (catalogJson) where

import Data.Aeson
import BotStrategy
import EnemySystem
import GameFactory (prepararPartida)
import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MapData
import MetaTypes
import ProgressionSystem (nivelMinimoModo, totalOndasPartidaModo, wavesHistoria)
import StableIds
import Tarefa3 (atualizaInimigo, atualizaJogo)
import TowerSystem
import TowerRuntime
import WaveSystem (aplicaMutadoresInfinito, criaOnda, ondasParaModo)

catalogJson :: Value
catalogJson =
  object
    [ "schema" .= String "immutable-towers-domain-catalog",
      "version" .= (1 :: Int),
      "towers" .= map towerSpecJson towerSpecsOrdenadas,
      "enemies" .= map (enemySpecJson . enemySpec) ([minBound .. maxBound] :: [EnemyClass]),
      "maps" .= map mapJson ([minBound .. maxBound] :: [MapId]),
      "modes" .= map modeJson allModes,
      "bot_references" .= map botReferenceJson allModes,
      "tower_upgrade_references" .= concatMap towerUpgradeReferences towerSpecsOrdenadas,
      "tower_price_references" .= map towerPriceReference allModes,
      "combat_hit_references"
        .= [combatHitReference towerSpecValue enemyClass | towerSpecValue <- towerSpecsOrdenadas, enemyClass <- [minBound .. maxBound]],
      "combat_sequence_references" .= map combatSequenceReference combatSequences,
      "enemy_update_references" .= map enemyUpdateReference enemyUpdateScenarios,
      "base_arrival_references" .= map baseArrivalReference baseArrivalScenarios
    ]
  where
    allModes = [ModoHistoria, ModoInfinito, ModoDesafio, ModoBoss, ModoSandbox]

towerSpecJson :: TowerSpec -> Value
towerSpecJson spec =
  object
    [ "id" .= towerId (towerIdSpec spec),
      "name" .= nomeTowerSpec spec,
      "rarity" .= rarityId (raridadeTowerSpec spec),
      "role" .= show (papelTowerSpec spec),
      "description" .= descricaoTowerSpec spec,
      "tags" .= map show (tagsTowerSpec spec),
      "priority" .= show (prioridadeTowerSpec spec),
      "color_rgb" .= colorJson (corTowerSpec spec),
      "shape" .= show (formaTowerSpec spec),
      "area" .= areaTowerSpec spec,
      "max_level" .= nivelMaximoTowerSpec spec,
      "price" .= precoTowerSpec spec,
      "base" .= towerJson (torreBaseSpec spec)
    ]

towerJson :: Torre -> Value
towerJson tower =
  object
    [ "damage" .= danoTorre tower,
      "range" .= alcanceTorre tower,
      "burst" .= rajadaTorre tower,
      "cycle" .= cicloTorre tower,
      "projectile"
        .= object
          [ "type_id" .= projectileId (tipoProjetil (projetilTorre tower)),
            "duration" .= durationJson (duracaoProjetil (projetilTorre tower))
          ]
    ]

enemySpecJson :: EnemySpec -> Value
enemySpecJson spec =
  object
    [ "id" .= enemyClassId (enemyClassSpec spec),
      "name" .= nomeEnemySpec spec,
      "tags" .= map show (tagsEnemySpec spec),
      "color_rgb" .= colorJson (corEnemySpec spec),
      "shape" .= show (formaEnemySpec spec),
      "health" .= vidaBaseEnemySpec spec,
      "speed" .= velocidadeBaseEnemySpec spec,
      "attack" .= ataqueBaseEnemySpec spec,
      "loot" .= butimBaseEnemySpec spec,
      "armor" .= armaduraEnemySpec spec,
      "direct_resistance" .= resistenciaDiretaEnemySpec spec,
      "area_resistance" .= resistenciaAreaEnemySpec spec,
      "regeneration" .= regeneracaoEnemySpec spec,
      "shield" .= escudoEnemySpec spec,
      "threat" .= ameacaEnemySpec spec
    ]

mapJson :: MapId -> Value
mapJson mid =
  let base = basePorMapa mid
      portal = portalPorMapa mid
      grid = mapaPorId mid
   in object
        [ "id" .= mapId mid,
          "width" .= maximum (0 : map length grid),
          "height" .= length grid,
          "grid" .= map (map terrainId) grid,
          "base"
            .= object
              [ "position" .= positionJson (posicaoBase base),
                "health" .= vidaBase base,
                "credits" .= creditosBase base
              ],
          "portal" .= object ["position" .= positionJson (posicaoPortal portal)]
        ]

modeJson :: ModoJogoEscolhido -> Value
modeJson mode =
  object
    [ "id" .= modeId mode,
      "minimum_level" .= nivelMinimoModo mode,
      "default_wave_count" .= totalOndasPartidaModo mode progressoInicial,
      "starting_health" .= startingHealth mode,
      "starting_credits" .= startingCredits mode,
      "waves" .= map waveJson (modeWaves mode),
      "infinite_reference_waves" .= infiniteReferences mode
    ]

modeWaves :: ModoJogoEscolhido -> [Onda]
modeWaves ModoHistoria = wavesHistoria progressoInicial
modeWaves mode = ondasParaModo mode

infiniteReferences :: ModoJogoEscolhido -> [Value]
infiniteReferences ModoInfinito =
  [ waveJson (aplicaMutadoresInfinito waveNumber (criaOnda (waveNumber + 1) (5 + waveNumber) 2))
    | waveNumber <- [1 .. 18]
  ]
infiniteReferences _ = []

startingHealth :: ModoJogoEscolhido -> Float
startingHealth ModoDesafio = 60
startingHealth ModoBoss = 110
startingHealth _ = 80

startingCredits :: ModoJogoEscolhido -> Int
startingCredits ModoSandbox = 999
startingCredits ModoDesafio = 125
startingCredits ModoBoss = 180
startingCredits ModoInfinito = 165
startingCredits ModoHistoria = 150

waveJson :: Onda -> Value
waveJson wave =
  object
    [ "cycle" .= cicloOnda wave,
      "entry_delay" .= entradaOnda wave,
      "enemies" .= map enemyInstanceJson (inimigosOnda wave)
    ]

enemyInstanceJson :: Inimigo -> Value
enemyInstanceJson enemy =
  object
    [ "class_id" .= enemyClassId (enemyClassOf enemy),
      "health" .= vidaInimigo enemy,
      "speed" .= velocidadeBaseInimigo enemy,
      "attack" .= ataqueInimigo enemy,
      "loot" .= butimInimigo enemy
    ]

positionJson :: Posicao -> Value
positionJson (x, y) = object ["x" .= x, "y" .= y]

colorJson :: (Int, Int, Int) -> Value
colorJson (red, green, blue) = object ["r" .= red, "g" .= green, "b" .= blue]

durationJson :: Duracao -> Value
durationJson value = case value of
  Finita seconds -> object ["kind" .= String "finite", "seconds" .= seconds]
  Infinita -> object ["kind" .= String "infinite"]

towerUpgradeReferences :: TowerSpec -> [Value]
towerUpgradeReferences spec =
  [ object
      [ "tower_id" .= towerId (towerIdSpec spec),
        "specialization_path" .= specializationId specialization,
        "states" .= upgradePathStates spec specialization
      ]
    | specialization <- [EspecializacaoA, EspecializacaoB]
  ]

upgradePathStates :: TowerSpec -> TowerSpecialization -> [Value]
upgradePathStates spec chosen = go initialRuntime (torreBaseSpec spec)
  where
    initialRuntime = TowerRuntime (towerIdSpec spec) 1 Nothing
    go runtime tower =
      towerUpgradeState spec chosen runtime tower
        : case nextUpgrade runtime tower of
          Nothing -> []
          Just (nextRuntime, nextTower) -> go nextRuntime nextTower
    nextUpgrade runtime tower
      | runtimeLevel runtime >= nivelMaximoTowerSpec spec = Nothing
      | precisaEspecializacao runtime = do
          upgraded <- upgradeComEspecializacao chosen runtime tower
          pure (TowerRuntime (runtimeTowerId runtime) (runtimeLevel runtime + 1) (Just chosen), upgraded)
      | otherwise = do
          upgraded <- upgradeTorreRuntime runtime tower
          pure (runtime {runtimeLevel = runtimeLevel runtime + 1}, upgraded)

towerUpgradeState :: TowerSpec -> TowerSpecialization -> TowerRuntime -> Torre -> Value
towerUpgradeState spec chosen runtime tower =
  object
    [ "level" .= runtimeLevel runtime,
      "specialization" .= fmap specializationId (runtimeSpecialization runtime),
      "upgrade_cost"
        .= if precisaEspecializacao runtime
          then custoEspecializacao chosen runtime tower
          else custoUpgradeRuntime runtime tower,
      "sale_value" .= valorVendaRuntime runtime tower,
      "stats" .= towerJson tower,
      "max_level" .= nivelMaximoTowerSpec spec
    ]

towerPriceReference :: ModoJogoEscolhido -> Value
towerPriceReference mode =
  let allUnlocked = progressoInicial {torresDesbloqueadas = map towerIdSpec towerSpecsOrdenadas}
   in object
        [ "mode_id" .= modeId mode,
          "prices"
            .= [ object ["tower_id" .= towerId (shopTowerId entry), "price" .= shopPrice entry]
                 | entry <- shopEntriesParaModo mode allUnlocked
               ]
        ]

combatHitReference :: TowerSpec -> EnemyClass -> Value
combatHitReference spec enemyClass =
  let position = (8.5, 6.5)
      tower = (torreBaseSpec spec) {posicaoTorre = position}
      registry = registerTower (towerIdSpec spec) position emptyTowerRegistry
      enemy = (criaInimigoClasse enemyClass 5) {posicaoInimigo = position}
      result = resolveTowerHitComContexto registry (contextoCombateInimigos [enemy]) tower enemy
   in object
        [ "tower_id" .= towerId (towerIdSpec spec),
          "enemy_id" .= enemyClassId enemyClass,
          "level" .= (5 :: Int),
          "health_before" .= vidaInimigo enemy,
          "health_after" .= vidaInimigo result,
          "effects" .= map effectJson (projeteisInimigo result)
        ]

effectJson :: Projetil -> Value
effectJson projectile =
  object
    [ "type_id" .= projectileId (tipoProjetil projectile),
      "duration" .= durationJson (duracaoProjetil projectile)
    ]

combatSequences :: [[TowerId]]
combatSequences =
  [ [Braseiro, Glaciar],
    [Braseiro, Sentinela],
    [Glaciar, Tesla],
    [Panico, Venenoide],
    [Venenoide, Sentinela],
    [Braseiro, Tesla],
    [Braseiro, Braseiro],
    [Sentinela, Braseiro, Tesla]
  ]

combatSequenceReference :: [TowerId] -> Value
combatSequenceReference towerIds =
  let position = (8.5, 6.5)
      initialEnemy = (criaInimigoClasse Basico 8) {posicaoInimigo = position}
      applyTower enemy towerIdValue =
        let spec = towerSpec towerIdValue
            tower = (torreBaseSpec spec) {posicaoTorre = position}
            registry = registerTower towerIdValue position emptyTowerRegistry
         in resolveTowerHitComContexto registry (contextoCombateInimigos [enemy]) tower enemy
      result = foldl applyTower initialEnemy towerIds
   in object
        [ "tower_ids" .= map towerId towerIds,
          "enemy_id" .= String "basico",
          "level" .= (8 :: Int),
          "health_before" .= vidaInimigo initialEnemy,
          "health_after" .= vidaInimigo result,
          "effects" .= map effectJson (projeteisInimigo result)
        ]

enemyUpdateScenarios :: [(String, Float, Posicao, [Projetil])]
enemyUpdateScenarios =
  [ ("basic-quarter-step", 0.25, (0.5, 2.5), []),
    ("fire-active", 0.25, (0.5, 2.5), [Projetil Fogo (Finita 1.0)]),
    ("poison-active", 0.25, (0.5, 2.5), [Projetil Veneno (Finita 1.0)]),
    ("resin-active", 0.25, (0.5, 2.5), [Projetil Resina (Finita 1.0)]),
    ("ice-active", 0.25, (0.5, 2.5), [Projetil Gelo (Finita 1.0)]),
    ("fire-expires-this-step", 1.0, (0.5, 2.5), [Projetil Fogo (Finita 0.5)]),
    ("resin-expires-this-step", 1.0, (0.5, 2.5), [Projetil Resina (Finita 0.5)])
  ]

enemyUpdateReference :: (String, Float, Posicao, [Projetil]) -> Value
enemyUpdateReference (scenarioId, delta, position, projectiles) =
  let initialEnemy =
        (criaInimigoClasse Basico 1)
          { posicaoInimigo = position,
            direcaoInimigo = Este,
            projeteisInimigo = projectiles
          }
      result = atualizaInimigo delta (mapaPorId PlanicieSerena) initialEnemy
   in object
        [ "id" .= scenarioId,
          "enemy_id" .= String "basico",
          "level" .= (1 :: Int),
          "delta" .= delta,
          "start_position" .= positionJson position,
          "health_before" .= vidaInimigo initialEnemy,
          "health_after" .= vidaInimigo result,
          "position_after" .= positionJson (posicaoInimigo result),
          "effects_before" .= map effectJson projectiles,
          "effects_after" .= map effectJson (projeteisInimigo result)
        ]

baseArrivalScenarios :: [(String, Float, Posicao)]
baseArrivalScenarios =
  [ ("already-on-base", 1 / 60, (35.5, 22.5)),
    ("short-step-before-base", 0.25, (34.6, 22.5)),
    ("half-step-crosses-base", 0.5, (34.6, 22.5)),
    ("full-step-crosses-base", 1.0, (34.6, 22.5)),
    ("large-step-crosses-base", 2.0, (33.6, 22.5))
  ]

baseArrivalReference :: (String, Float, Posicao) -> Value
baseArrivalReference (scenarioId, delta, position) =
  let (preparedGame, _, _, _) = prepararPartida ModoHistoria progressoInicial
      initialEnemy =
        (criaInimigoClasse Basico 1)
          { posicaoInimigo = position,
            direcaoInimigo = Este
          }
      isolatedGame =
        preparedGame
          { portaisJogo = [],
            torresJogo = [],
            inimigosJogo = [initialEnemy]
          }
      result = atualizaJogo delta isolatedGame
   in object
        [ "id" .= scenarioId,
          "enemy_id" .= String "basico",
          "level" .= (1 :: Int),
          "delta" .= delta,
          "start_position" .= positionJson position,
          "base_health_before" .= vidaBase (baseJogo isolatedGame),
          "base_health_after" .= vidaBase (baseJogo result),
          "remaining_enemies" .= length (inimigosJogo result),
          "enemy_position_after" .= case inimigosJogo result of
            enemy : _ -> Just (positionJson (posicaoInimigo enemy))
            [] -> Nothing
        ]

botReferenceJson :: ModoJogoEscolhido -> Value
botReferenceJson mode =
  let (game, mid, _, _) = prepararPartida mode progressoInicial
      decision = decideBotJogo game
   in object
        [ "mode_id" .= modeId mode,
          "map_id" .= mapId mid,
          "action" .= strategicActionJson (strategicAction decision),
          "score" .= strategicScore decision
        ]

strategicActionJson :: StrategicAction -> Value
strategicActionJson action = case action of
  StrategicBuild tid (x, y) -> object ["kind" .= String "build", "tower_id" .= towerId tid, "cell" .= object ["x" .= (floor x :: Int), "y" .= (floor y :: Int)]]
  StrategicUpgrade (x, y) -> object ["kind" .= String "upgrade", "cell" .= object ["x" .= (floor x :: Int), "y" .= (floor y :: Int)]]
  StrategicSave -> object ["kind" .= String "save"]
  StrategicNone -> object ["kind" .= String "none"]
