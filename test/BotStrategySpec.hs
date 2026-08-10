module BotStrategySpec (testesBotStrategy) where

import BotStrategy
import LI12425
import Test.HUnit
import TowerSystem

testesBotStrategy :: Test
testesBotStrategy = TestLabel "Strategic bot" $ test ["decision is deterministic" ~: determinismo, "bot respects affordable action" ~: economia]

jogoTeste :: Jogo
jogoTeste = Jogo (Base 80 (0.5, 0.5) 100) [] [torreResinaBase] [[Terra, Relva, Relva], [Relva, Relva, Relva]] [] [(44, torreResinaBase)]

determinismo :: Assertion
determinismo = decideBotJogo jogoTeste @?= decideBotJogo jogoTeste

economia :: Assertion
economia = case strategicAction (decideBotJogo jogoTeste) of
  StrategicBuild _ _ -> return ()
  StrategicUpgrade _ -> return ()
  StrategicSave -> return ()
  StrategicNone -> return ()
