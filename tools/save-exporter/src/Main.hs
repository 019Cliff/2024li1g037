{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AccountStorage (SaveError, loadAccount, loadAccountIndex)
import AccountTypes
import CatalogExport (catalogJson)
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import ImmutableTowers
import LI12425
import MetaTypes
import SaveSystem (carregarJogoLocal, carregarMetaEstado)
import ShopSystem (ShopReward (..))
import StableIds
import System.Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import TowerRuntime

main :: IO ()
main = do
  command <- parseArgs =<< getArgs
  case command of
    ExportCatalog outputPath -> writeResult "Catalog" outputPath catalogJson
    ExportSaves inputRoot outputPath -> do
      inputExists <- doesDirectoryExist inputRoot
      if not inputExists
        then die ("Input directory does not exist: " ++ inputRoot)
        else do
          package <- withCurrentDirectory inputRoot buildTransfer
          writeResult "Transfer" outputPath package

data Command
  = ExportSaves FilePath FilePath
  | ExportCatalog FilePath

parseArgs :: [String] -> IO Command
parseArgs ("--catalog-output" : outputPath : []) = pure (ExportCatalog outputPath)
parseArgs args = go args "." "migration/samples/immutable-towers-transfer-v1.json"
  where
    go [] input output = pure (ExportSaves input output)
    go ("--input" : value : rest) _ output = go rest value output
    go ("--output" : value : rest) input _ = go rest input value
    go _ _ _ = die "Usage: immutable-towers-save-exporter [--input DIR] [--output FILE] | --catalog-output FILE"

writeResult :: String -> FilePath -> Value -> IO ()
writeResult label outputPath value = do
  result <- writeAtomicJson outputPath value
  either die (const (putStrLn (label ++ " written to " ++ outputPath))) result

buildTransfer :: IO Value
buildTransfer = do
  now <- getCurrentTime
  hasMeta <- doesFileExist "immutable-towers-meta.txt"
  hasGame <- doesFileExist "immutable-towers-save.txt"
  meta <- if hasMeta then Just <$> carregarMetaEstado else pure Nothing
  game <- if hasGame then carregarJogoLocal else pure Nothing
  (indexValue, accountValues, accountWarnings) <- exportAccounts
  let legacy = legacyGlobalJson meta game
      warnings =
        accountWarnings
          ++ ["game save could not be decoded" | hasGame && game == Nothing]
          ++ ["legacy enemy class is not explicit; runtime stats were preserved" | game /= Nothing]
  pure $
    object
      [ "schema" .= String "immutable-towers-transfer",
        "version" .= (1 :: Int),
        "source"
          .= object
            [ "game" .= String "haskell-gloss",
              "exporter_version" .= (1 :: Int),
              "exported_at_utc" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
            ],
        "legacy_global" .= legacy,
        "account_index" .= indexValue,
        "accounts" .= accountValues,
        "warnings" .= warnings
      ]

exportAccounts :: IO (Maybe Value, [Value], [String])
exportAccounts = do
  root <- getAppUserDataDirectory "ImmutableTowers"
  exists <- doesFileExist (root </> "accounts-index-v1.txt")
  if not exists
    then pure (Nothing, [], [])
    else do
      indexResult <- loadAccountIndex
      case indexResult of
        Left err -> pure (Nothing, [], ["account index: " ++ showSaveError err])
        Right index -> do
          results <- forM (contasIndexadas index) $ \summary -> do
            loaded <- loadAccount (accountIdSummary summary)
            pure $ case loaded of
              Left err -> Left (accountIdTexto (accountIdSummary summary) ++ ": " ++ showSaveError err)
              Right account -> Right (accountJson account)
          let values = [value | Right value <- results]
              warnings = [warning | Left warning <- results]
          pure (Just (accountIndexJson index), values, warnings)

showSaveError :: SaveError -> String
showSaveError = show

legacyGlobalJson :: Maybe (PerfilJogador, [Pontuacao], ModoJogoEscolhido, MetaProgress) -> Maybe (Jogo, TowerRegistry) -> Maybe Value
legacyGlobalJson Nothing Nothing = Nothing
legacyGlobalJson meta game =
  Just $
    object
      [ "meta" .= fmap metaTupleJson meta,
        "pending_run" .= fmap (uncurry gameJson) game
      ]

metaTupleJson :: (PerfilJogador, [Pontuacao], ModoJogoEscolhido, MetaProgress) -> Value
metaTupleJson (profile, scores, mode, progress) =
  object
    [ "profile" .= profileJson profile,
      "leaderboard" .= map scoreJson scores,
      "selected_mode" .= modeId mode,
      "meta_progress" .= metaProgressJson progress
    ]

accountIndexJson :: AccountIndex -> Value
accountIndexJson index =
  object
    [ "version" .= versaoIndiceContas index,
      "accounts" .= map accountSummaryJson (contasIndexadas index),
      "last_account_id" .= fmap accountIdTexto (ultimaContaUsada index),
      "remember_account" .= lembrarConta index
    ]

accountSummaryJson :: AccountSummary -> Value
accountSummaryJson summary =
  object
    [ "account_id" .= accountIdTexto (accountIdSummary summary),
      "name" .= nomeContaSummary summary,
      "level" .= nivelSummary summary,
      "last_map_id" .= fmap mapId (ultimoMapaSummary summary)
    ]

accountJson :: AccountData -> Value
accountJson account =
  object
    [ "version" .= versaoConta account,
      "account_id" .= accountIdTexto (accountId account),
      "name" .= nomeConta account,
      "profile" .= profileJson (perfilConta account),
      "meta_progress" .= metaProgressJson (progressoConta account),
      "leaderboard" .= map scoreJson (leaderboardConta account),
      "selected_mode" .= modeId (modoSelecionadoConta account),
      "shop_state" .= metaProgressJson (estadoLojaConta account),
      "pending_run" .= fmap (uncurry gameJson) (partidaPendenteConta account),
      "pending_reward" .= fmap rewardJson (recompensaPendenteConta account)
    ]

profileJson :: PerfilJogador -> Value
profileJson profile =
  object
    [ "name" .= nomeJogador profile,
      "games" .= jogosJogador profile,
      "wins" .= vitoriasJogador profile,
      "losses" .= derrotasJogador profile,
      "best_score" .= melhorPontuacaoJogador profile
    ]

scoreJson :: Pontuacao -> Value
scoreJson score =
  object
    [ "name" .= nomePontuacao score,
      "mode_id" .= modeId (modoPontuacao score),
      "score" .= valorPontuacao score,
      "waves" .= ondasPontuacao score
    ]

metaProgressJson :: MetaProgress -> Value
metaProgressJson progress =
  object
    [ "gems" .= gemasJogador progress,
      "level" .= nivelJogadorMeta progress,
      "unlocked_tower_ids" .= map towerId (torresDesbloqueadas progress),
      "fused_tower_ids" .= map towerId (torresFundidas progress),
      "history_chapter" .= capituloHistoriaAtual progress,
      "history_stage" .= estagioHistoriaAtual progress,
      "completed_stages" .= estagiosConcluidos progress,
      "map_rotation" .= rotacaoMapasAtual progress
    ]

gameJson :: Jogo -> TowerRegistry -> Value
gameJson currentGame registry =
  object
    [ "base" .= baseJson (baseJogo currentGame),
      "portals" .= map portalJson (portaisJogo currentGame),
      "towers" .= map (towerJson registry) (torresJogo currentGame),
      "map_grid" .= map (map terrainId) (mapaJogo currentGame),
      "active_enemies" .= map enemyJson (inimigosJogo currentGame),
      "shop" .= map (\(cost, tower) -> object ["cost" .= cost, "tower" .= towerJson emptyTowerRegistry tower]) (lojaJogo currentGame)
    ]

baseJson :: Base -> Value
baseJson base = object ["health" .= vidaBase base, "position" .= positionJson (posicaoBase base), "credits" .= creditosBase base]

portalJson :: Portal -> Value
portalJson portal = object ["position" .= positionJson (posicaoPortal portal), "waves" .= map waveJson (ondasPortal portal)]

waveJson :: Onda -> Value
waveJson wave =
  object
    [ "enemies" .= map enemyJson (inimigosOnda wave),
      "cycle" .= cicloOnda wave,
      "remaining_cycle" .= tempoOnda wave,
      "entry_delay" .= entradaOnda wave
    ]

towerJson :: TowerRegistry -> Torre -> Value
towerJson registry tower =
  object
    [ "position" .= positionJson (posicaoTorre tower),
      "damage" .= danoTorre tower,
      "range" .= alcanceTorre tower,
      "burst" .= rajadaTorre tower,
      "cycle" .= cicloTorre tower,
      "remaining_cycle" .= tempoTorre tower,
      "projectile" .= projectileJson (projetilTorre tower),
      "runtime" .= fmap towerRuntimeJson (lookupTowerRuntime (posicaoTorre tower) registry)
    ]

towerRuntimeJson :: TowerRuntime -> Value
towerRuntimeJson runtime =
  object
    [ "tower_id" .= towerId (runtimeTowerId runtime),
      "level" .= runtimeLevel runtime,
      "specialization" .= fmap specializationId (runtimeSpecialization runtime)
    ]

enemyJson :: Inimigo -> Value
enemyJson enemy =
  object
    [ "position" .= positionJson (posicaoInimigo enemy),
      "direction" .= directionId (direcaoInimigo enemy),
      "health" .= vidaInimigo enemy,
      "base_speed" .= velocidadeBaseInimigo enemy,
      "speed" .= velocidadeInimigo enemy,
      "attack" .= ataqueInimigo enemy,
      "loot" .= butimInimigo enemy,
      "effects" .= map projectileJson (projeteisInimigo enemy)
    ]

projectileJson :: Projetil -> Value
projectileJson projectile = object ["type_id" .= projectileId (tipoProjetil projectile), "duration" .= durationJson (duracaoProjetil projectile)]

durationJson :: Duracao -> Value
durationJson duration = case duration of
  Finita seconds -> object ["kind" .= String "finite", "seconds" .= seconds]
  Infinita -> object ["kind" .= String "infinite"]

rewardJson :: ShopReward -> Value
rewardJson reward = case reward of
  NovaTorre tid -> object ["kind" .= String "new_tower", "tower_id" .= towerId tid]
  CompensacaoDuplicado tid gems -> object ["kind" .= String "duplicate_compensation", "tower_id" .= towerId tid, "gems" .= gems]
  RecompensaColecaoCompleta gems -> object ["kind" .= String "collection_complete", "gems" .= gems]

positionJson :: Posicao -> Value
positionJson (x, y) = object ["x" .= x, "y" .= y]

writeAtomicJson :: FilePath -> Value -> IO (Either String ())
writeAtomicJson path value = do
  let temporary = path ++ ".tmp"
      backup = path ++ ".bak"
      payload = encode value
  result <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile temporary payload
    validation <- eitherDecodeFileStrict temporary :: IO (Either String Value)
    case validation of
      Left err -> ioError (userError ("temporary JSON validation failed: " ++ err))
      Right _ -> do
        exists <- doesFileExist path
        if exists then copyFile path backup else pure ()
        renameFile temporary path
  pure $ case result of
    Left err -> Left (show (err :: IOException))
    Right () -> Right ()
