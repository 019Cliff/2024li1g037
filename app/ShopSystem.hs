module ShopSystem
  ( ShopReward (..),
    ChestPurchase (..),
    ShopTab (..),
    shopPool,
    compraBau,
    fusaoTempestade,
    custoFusaoTempestade,
  )
where

import MetaTypes

data ShopReward
  = NovaTorre TowerId
  | CompensacaoDuplicado TowerId Int
  | RecompensaColecaoCompleta Int
  deriving (Show, Read, Eq)

data ChestPurchase = ChestPurchase
  { metaDepoisCompra :: MetaProgress,
    recompensaCompra :: ShopReward,
    seedLojaSeguinte :: Int
  }
  deriving (Show, Read, Eq)

data ShopTab = Baús | Colecao | Fusao
  deriving (Show, Read, Eq, Enum, Bounded)

custoFusaoTempestade :: Int
custoFusaoTempestade = 180

shopPool :: ChestType -> [TowerId]
shopPool bau = case bau of
  BauMadeira -> [Sentinela, Glaciar, Braseiro]
  BauCristal -> [Glaciar, Braseiro, Panico, Venenoide, Tesla, Impacto]
  BauImperial -> [Panico, Venenoide, Tesla, Impacto, Solar, Tempestade]

compraBau :: ChestType -> MetaProgress -> Either String ChestPurchase
compraBau bau meta
  | gemasJogador meta < custo = Left "Gemas insuficientes"
  | otherwise =
      let candidatos = shopPool bau
          desbloqueadas = torresDesbloqueadas meta
          seed = proximaSeed meta bau
          selecionada = candidatos !! (seed `mod` length candidatos)
          (recompensa, desbloqueadasNovas, compensacao) =
            if selecionada `elem` desbloqueadas
              then (CompensacaoDuplicado selecionada compensacaoDuplicado, desbloqueadas, compensacaoDuplicado)
              else (NovaTorre selecionada, desbloqueadas ++ [selecionada], 0)
          gemasFinais = gemasJogador meta - custo + compensacao
          metaNova = meta {gemasJogador = gemasFinais, torresDesbloqueadas = desbloqueadasNovas}
       in Right (ChestPurchase metaNova recompensa (seed + 1))
  where
    custo = custoBau bau
    compensacaoDuplicado = max 5 (custo `div` 4)

proximaSeed :: MetaProgress -> ChestType -> Int
proximaSeed meta bau =
  gemasJogador meta
    + nivelJogadorMeta meta * 13
    + estagiosConcluidos meta * 17
    + fromEnum bau * 29
    + rotacaoMapasAtual meta * 7

fusaoTempestade :: MetaProgress -> Either String MetaProgress
fusaoTempestade meta
  | Tempestade `elem` torresDesbloqueadas meta = Left "Tempestade ja desbloqueada"
  | Tesla `notElem` torresDesbloqueadas meta || Solar `notElem` torresDesbloqueadas meta = Left "Falta Tesla e Solar"
  | gemasJogador meta < custoFusaoTempestade = Left "Faltam 180 gemas"
  | otherwise =
      Right
        meta
          { gemasJogador = gemasJogador meta - custoFusaoTempestade,
            torresDesbloqueadas = torresDesbloqueadas meta ++ [Tempestade],
            torresFundidas = Tempestade : torresFundidas meta,
            nivelJogadorMeta = max (nivelJogadorMeta meta) 6
          }
