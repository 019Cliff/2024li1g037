module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1  
import LI12425
    

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "validaJogo" ~: True ~=? validaJogo jogo1,
        "validaPortais" ~: True ~=? validaPortais jogo1,
        "posicaoPortalValida" ~: True ~=? posicaoPortalValida (0, 0) mapa1,
        "validaOndaPortal" ~: True ~=? validaOndaPortal portal1,
        "ondaSemInimigos" ~: True ~=? ondaSemInimigos (Onda [] 1 1 1),
        "minimoPortal" ~: True ~=? minimoPortal [portal1],
        "terrenoPorPosicao" ~: Just Terra ~=? terrenoPorPosicao (0, 0) mapa1,
        "posicionadoEmTerra" ~: True ~=? posicionadoEmTerra mapa1 [portal1],
        "posicionadoEmRelvaTorre" ~: True ~=? posicionadoEmRelvaTorre mapa1 [torre1],
        "naoSobrepostosTorreBase" ~: True ~=? naoSobrepostosTorreBase [] base1 [torre1] [portal1] mapa1,
        "verificaPosicaoTorreEmPortal" ~: True ~=? verificaPosicaoTorreEmPortal mapa1 [torre1] [portal1],
        "verificaPosicaoBaseEmPortal" ~: True ~=? verificaPosicaoBaseEmPortal mapa1 base1 [portal1],
        "maximoOndaPorPortal" ~: True ~=? maximoOndaPorPortal [portal1],
        "caminhoPortalBase" ~: True ~=? caminhoPortalBase mapa1 [(1, 1), (2, 2)] (0, 0),
        "buscaCaminho" ~: True ~=? buscaCaminho mapa1 (1, 1) (0, 0) [],
        "adjacentes" ~: [(0.0, 1.0), (2.0, 1.0), (1.0, 0.0), (1.0, 2.0)] ~=? adjacentes (1.0, 1.0),
        "posicaoValida" ~: True ~=? posicaoValida mapa1 (1.5, 2.5),
        "validaInimigos" ~: True ~=? validaInimigos jogo1,
        "validaTorres" ~: True ~=? validaTorres jogo1,
        "todasEmRelva" ~: True ~=? todasEmRelva (mapaJogo jogo1) (torresJogo jogo1),
        "alcancesPositivos" ~: True ~=? alcancesPositivos (torresJogo jogo1),
        "rajadasPositivas" ~: True ~=? rajadasPositivas (torresJogo jogo1),
        "ciclosNaoNegativos" ~: True ~=? ciclosNaoNegativos (torresJogo jogo1),
        "naoSobrepostas" ~: True ~=? naoSobrepostas (torresJogo jogo1),
        "validaBase" ~: True ~=? validaBase jogo1
      ] 

