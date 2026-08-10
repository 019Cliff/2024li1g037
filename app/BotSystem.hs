module BotSystem
  ( botColocaTorre,
    botExecutaAcao,
    sugestaoBot,
  )
where

import LI12425
import BotStrategy

botColocaTorre :: Jogo -> Jogo
botColocaTorre jogoAtual = maybe jogoAtual fst (botExecutaAcao jogoAtual)

botExecutaAcao :: Jogo -> Maybe (Jogo, String)
botExecutaAcao jogoAtual =
  let decisao = decideBotJogo jogoAtual
   in case applyStrategicAction jogoAtual (strategicAction decisao) of
        Nothing -> Nothing
        Just (jogoNovo, texto) -> Just (jogoNovo, texto ++ " | " ++ strategicReason decisao)

sugestaoBot :: Jogo -> String
sugestaoBot jogoAtual = strategicReason (decideBotJogo jogoAtual)
