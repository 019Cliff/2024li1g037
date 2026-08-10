module AccountTypes
  ( AccountId (..),
    LocalCredential (..),
    AccountSummary (..),
    AccountData (..),
    AccountIndex (..),
    AccountSession (..),
    accountIdTexto,
    normalizarNomeConta,
    nomeContaValido,
  )
where

import ImmutableTowers
import LI12425 (Jogo)
import MetaTypes
import ShopSystem (ShopReward)
import TowerRuntime

newtype AccountId = AccountId String
  deriving (Show, Read, Eq, Ord)

data LocalCredential = SemCredencial
  deriving (Show, Read, Eq)

data AccountSummary = AccountSummary
  { accountIdSummary :: AccountId,
    nomeContaSummary :: String,
    nivelSummary :: Int,
    ultimoMapaSummary :: Maybe MapId
  }
  deriving (Show, Read, Eq)

data AccountData = AccountData
  { versaoConta :: Int,
    accountId :: AccountId,
    nomeConta :: String,
    credencialConta :: LocalCredential,
    perfilConta :: PerfilJogador,
    progressoConta :: MetaProgress,
    leaderboardConta :: [Pontuacao],
    modoSelecionadoConta :: ModoJogoEscolhido,
    estadoLojaConta :: MetaProgress,
    partidaPendenteConta :: Maybe (Jogo, TowerRegistry),
    recompensaPendenteConta :: Maybe ShopReward
  }
  deriving (Show, Read, Eq)

data AccountIndex = AccountIndex
  { versaoIndiceContas :: Int,
    contasIndexadas :: [AccountSummary],
    ultimaContaUsada :: Maybe AccountId,
    lembrarConta :: Bool
  }
  deriving (Show, Read, Eq)

data AccountSession = AccountSession
  { dadosContaSessao :: AccountData,
    dirtyAccountSession :: Bool
  }
  deriving (Show, Read, Eq)

accountIdTexto :: AccountId -> String
accountIdTexto (AccountId value) = value

normalizarNomeConta :: String -> String
normalizarNomeConta = unwords . words . map normalizarChar
  where
    normalizarChar c
      | c >= 'A' && c <= 'Z' = toLowerAscii c
      | otherwise = c
    toLowerAscii c = toEnum (fromEnum c + (fromEnum 'a' - fromEnum 'A'))

nomeContaValido :: String -> Either String String
nomeContaValido nome
  | null limpo = Left "O nome da conta nao pode ficar vazio"
  | length limpo > 16 = Left "O nome da conta pode ter no maximo 16 caracteres"
  | otherwise = Right limpo
  where
    limpo = unwords (words nome)
