module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import Tarefa2
import Tarefa1
import LI12425


testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ "atualizaJogo" ~:            ~=? 2.0 jogo1,   
        "disparosTorre" ~:           ~=? torre3 [inimigo1, inimigo2],
        "atualizaTorre" ~: Torre { posicaoTorre = (3, 1), alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 4.0, projetilTorre = projetil1 } ~=? 1.0 torre3,        
        "atacaInimigo" ~: [inimigo1,inimigo2, inimigo3] ~=? torre1 [inimigo1, inimigo2, inimigo3],
        "torresAtualizadas" ~: True ~=? not False
        "inimigosAtingidos" ~: [inimigo1, inimigo2] ~=? torre3 [inimigo1, inimigo2, inimigo3]
        "deInimigoABase" ~: True ~=? not False
        "criarDirecoes" ~: True ~=? not False      
        "calculaOPercurso" ~: True ~=? not False
        "dePosicaoParaDirecao" ~: True ~=? not False       
        "dePosicoesParaDirecoes" ~: [Norte, Este] ~=? [(0,0), (0,1), (2,1)]
        "movimenta" ~: (3,1) ~=? (2,1) Este
        "atualizaMovimentoInimigo" ~: True ~=? inimigo1 [Norte, Sul, Este]~
        "projeteisalteramVelocidade" ~: True ~=? not False      
        "alteraVelocidade" ~: True ~=? not False
        "acabaEfeito" ~: True ~=? not False       
        "duracaoReduzida" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0 }, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}] ~=? [projetil1,projetil2] 1.0,
        "atualizaProjetilComOTempo" ~: Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0 } ~=? projetil1 3.0,      
        "filtraProjeteisExpirados" ~: [projetil1, projetil2] ~=? [projetil1, projetil2, projetil4],
        "extraiButins" ~: [50, 50, 100] ~=? [inimigo1, inimigo2, inimigo3],
        "adicionaOsCreditosABase" ~: Base { posicaoBase = (1, 1), creditosBase = 300, vidaBase = 100.0 } ~=?  [50,50,100] base1,    
        "adicionaButimAosCreditosInimigos" ~: Base { posicaoBase = (1, 1), creditosBase = 300, vidaBase = 100.0 } ~=? [inimigo1, inimigo2] base1,  
        "retiraInimigoEmFunçaoDaVida" ~: [inimigo1] ~=? [inimigo1, inimigo4],
        "eliminaDaListaDeAtivos" ~:           ~=? ,
        "atualizaInimigo" ~:           ~=? torre3 [inimigo1, inimigo2],
        "atualizaBase" ~: Base { posicaoBase = (1, 1), creditosBase = 150, vidaBase = 100.0 } ~=? base1 6.0 [inimigo4] ,
        "verificainimigoAtingeBase" ~: True ~=? inimigo2                            ,     
        "ajustaDistancia" ~:           ~=? ,
        "portalComOndasAtivas" ~: Portal { posicaoPortal = (0, 3), ondasPortal = [onda1]} ~=? portal2,
        "portaisComOndasAtivas" ~:            ~=? [portal1, portal2],
        "ordemNaturalDosInimigos" ~: [inimigo2,inimigo3] ~=? onda1,
        "atualizaOnda" ~: Onda {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 2.0, entradaOnda = 5.0 }  ~=? 4.0 onda1,
        "atualizaPortal" ~: Portal { posicaoPortal = (0, 3), ondasPortal = [Onda {inimigosOnda = [inimigo2,inimigo3], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 }
                            ,Onda {inimigosOnda = [inimigo5], cicloOnda = 2.0, tempoOnda = 1.0, entradaOnda = 5.0 } ]} ~=? 2.0 portal2
      ]

