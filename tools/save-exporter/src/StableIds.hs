module StableIds
  ( modeId,
    mapId,
    towerId,
    specializationId,
    terrainId,
    directionId,
    projectileId,
    enemyClassId,
    rarityId,
  )
where

import EnemySystem (EnemyClass (..))
import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425 (Direcao (..), Terreno (..), TipoProjetil (..))
import MetaTypes (MapId (..), Raridade (..), TowerId (..))
import TowerRuntime (TowerSpecialization (..))

modeId :: ModoJogoEscolhido -> String
modeId value = case value of
  ModoHistoria -> "history"
  ModoInfinito -> "infinite"
  ModoDesafio -> "challenge"
  ModoBoss -> "boss"
  ModoSandbox -> "sandbox"

mapId :: MapId -> String
mapId value = case value of
  PlanicieSerena -> "planicie_serena"
  GargantaPedra -> "garganta_pedra"
  LagoFraturado -> "lago_fraturado"
  CruzamentoSolar -> "cruzamento_solar"
  BastiaoEspiral -> "bastiao_espiral"

towerId :: TowerId -> String
towerId value = case value of
  Sentinela -> "sentinela"
  Glaciar -> "glaciar"
  Braseiro -> "braseiro"
  Panico -> "panico"
  Venenoide -> "venenoide"
  Tesla -> "tesla"
  Impacto -> "impacto"
  Solar -> "solar"
  Tempestade -> "tempestade"

specializationId :: TowerSpecialization -> String
specializationId value = case value of
  EspecializacaoA -> "a"
  EspecializacaoB -> "b"

terrainId :: Terreno -> String
terrainId value = case value of
  Relva -> "grass"
  Terra -> "path"
  Asfalto -> "asphalt"
  Agua -> "water"

directionId :: Direcao -> String
directionId value = case value of
  Norte -> "north"
  Sul -> "south"
  Este -> "east"
  Oeste -> "west"

projectileId :: TipoProjetil -> String
projectileId value = case value of
  Fogo -> "fire"
  Gelo -> "ice"
  Resina -> "resin"
  Medo -> "fear"
  Veneno -> "poison"
  Eletrico -> "electric"

enemyClassId :: EnemyClass -> String
enemyClassId value = case value of
  Basico -> "basico"
  Rapido -> "rapido"
  Tanque -> "tanque"
  Blindado -> "blindado"
  Regenerador -> "regenerador"
  Dispersor -> "dispersor"
  Protegido -> "protegido"
  Elite -> "elite"
  BossAcelerador -> "boss_acelerador"
  BossGuardiao -> "boss_guardiao"
  BossRuptura -> "boss_ruptura"

rarityId :: Raridade -> String
rarityId value = case value of
  Comum -> "common"
  Raro -> "rare"
  Epico -> "epic"
  Lendario -> "legendary"
  Mitico -> "mythic"
