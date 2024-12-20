module Tarefa2Spec  where

import Test.HUnit
import Tarefa2
import LI12425
import Tarefa1

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "inimigosNoAlcance - sem inimigos no alcance" ~: [] ~=? inimigosNoAlcance torre1 [],
        "inimigosNoAlcance - com inimigos no alcance" ~: [inimigo1] ~=? inimigosNoAlcance torre1 [inimigo1],
        "atingeInimigo - inimigo com dano" ~: inimigo1 {vidaInimigo = 99.0, projeteisInimigo = [projetil1]} ~=? atingeInimigo torre1 inimigo1,
        "somaDuracao - duracao finita" ~: Finita 8 ~=? somaDuracao (Finita 3) (Finita 5),
        "somaDuracao - duracao infinita" ~: Infinita ~=? somaDuracao Infinita (Finita 5),
        "fogoEGelo - sem Fogo e Gelo" ~: [projetil1] ~=? fogoEGelo [projetil1],
        "fogoEGelo - com Fogo e Gelo" ~: [] ~=? fogoEGelo [projetil1, Projetil Gelo (Finita 3)],
        "atingeFogoEResina - sem Resina" ~: [projetil1] ~=? atingeFogoEResina [projetil1],
        "atingeFogoEResina - com Fogo e Resina" ~: [projetil1 {duracaoProjetil = Finita 10}] ~=? atingeFogoEResina [projetil1, Projetil Resina (Finita 2)],
        "ativaInimigo - sem ondas" ~: (portal1, []) ~=? ativaInimigo portal1 [],
        "ativaInimigo - com ondas" ~: (portal1 {ondasPortal = [Onda [] 0 0 0]}, [inimigo1]) ~=? ativaInimigo portalComOnda [],
        "terminouJogo - jogo ganho" ~: True ~=? terminouJogo jogo1 {inimigosJogo = []},
        "terminouJogo - jogo perdido" ~: True ~=? terminouJogo jogo1 {baseJogo = base1 {vidaBase = 0}}
        
      ]
  where
    portalComOnda = portal1 {ondasPortal = [Onda [inimigo1] 0 0 0]}
