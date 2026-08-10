module AccountSystem
  ( novaConta,
    renomearConta,
    nomeJaUsado,
    resumoConta,
  )
where

import AccountTypes
import ImmutableTowers
import MetaTypes

novaConta :: AccountId -> String -> AccountData
novaConta aid nome =
  AccountData
    { versaoConta = 1,
      accountId = aid,
      nomeConta = nome,
      credencialConta = SemCredencial,
      perfilConta = perfilInicial {nomeJogador = nome},
      progressoConta = progressoInicial,
      leaderboardConta = [],
      modoSelecionadoConta = ModoHistoria,
      estadoLojaConta = progressoInicial,
      partidaPendenteConta = Nothing,
      recompensaPendenteConta = Nothing
    }

renomearConta :: String -> AccountData -> Either String AccountData
renomearConta nome conta = do
  nomeValido <- nomeContaValido nome
  return
    conta
      { nomeConta = nomeValido,
        perfilConta = (perfilConta conta) {nomeJogador = nomeValido}
      }

nomeJaUsado :: String -> [AccountSummary] -> Bool
nomeJaUsado nome = any ((== normalizarNomeConta nome) . normalizarNomeConta . nomeContaSummary)

resumoConta :: AccountData -> AccountSummary
resumoConta conta =
  (AccountSummary (accountId conta) (nomeConta conta) (nivelJogadorMeta (progressoConta conta)) Nothing)
