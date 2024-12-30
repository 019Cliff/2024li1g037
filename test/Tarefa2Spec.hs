module Tarefa2Spec (testesTarefa2) where

import Test.HUnit

import Tarefa2
import LI12425


testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "inimigosNoAlcance (sem inimigos)" ~: [] ~=? inimigosNoAlcance (torre (1.5, 1.5) 2) [], 
        "inimigosNoAlcance (com 1 inimigo)" ~:[Inimigo (0.5, 0.5)] ~=? inimigosNoAlcance (torre (1.5, 1.5) 2) [inimigo (0.5, 0.5)],
        "inimigosNoAlcance (com 3 inimigos)" ~: [Inimigo (0.5, 0.5), Inimigo (1.5, 1.5)] ~=? inimigosNoAlcance (torre (1.5, 1.5) 2) [Inimigo (0.5, 0.5), Inimigo (4.5, 4.5), Inimigo (1.5, 1.5)] ,
        "atingeInimigo (sinergia de FogoEGelo) " ~:  ~=? ,
        "atingeInimigo (sinergia de FogoEResina)" ~:  ~=? , 
        "atingeInimigo (sinergia de ProjeteisIguais)" ~:  ~=? ,
        "atualizaLista" ~: [Resina, Gelo] ~=? [Resina] Gelo,
        "fogoEGelo" ~: [Resina] ~=? [Fogo, Resina, Gelo],
        "fogoEGelo (com lista vazia)" ~:  []  ~=? [],
        "encontraGelo" ~: [Gelo, Gelo, Gelo] ~=? [Gelo, Resina, Gelo, Gelo, Fogo, Fogo],
        "encontraGelo (com lista vazia)" ~: [] ~=? [],
        "atingeFogoEResina" ~: [Fogo] ~=? [Gelo, Resina, Gelo, Gelo, Fogo],
        "atingeFogoEResina" ~: [Gelo, Resina, Gelo] ~=? [Gelo, Resina, Gelo],
        "encontraFogo" ~: [Fogo, Fogo] ~=? [Gelo, Resina, Gelo, Gelo, Fogo, Fogo],
        "encontraFogo (com lista vazia)" ~: [] ~=? [],
        "encontraFogo " ~: [Resina] ~=? [Gelo, Resina, Gelo, Gelo, Fogo, Fogo],
        "encontraResina (com lista vazia)" ~: [] ~=? [],
        "duplicaDuracao" ~: Finita 10.0 ~=? Finita 5.0,
        "duplicaDuracao (com duraçao infinita) " ~: Infinita ~=? Infinita,
        "dividePorTipoProjetil" ~: ([Fogo,Fogo,Fogo,Fogo],[Gelo,Gelo],[Resina,Resina]) ~=? [Fogo, Gelo, Fogo, Fogo, Resina, Fogo, Gelo, Resina],
        "somaDuracaoDeListaProjeteisIguais (Com dois fogos)" ~: Finita 10.0 ~=? [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }],
        "somaDuracaoDeListaProjeteisIguais (Com dois gelos)" ~: Finita 8.0 ~=? [Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }],   
        "somaDuracaoDeListaProjeteisIguais (Com dois diferentes)" ~: Finita 9.0 ~=?  [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }],
        " extraiDuracoes (com dois com duraçao finita) " ~: [Finita 5.0, Finita 4.0] ~=?  [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }],
        " extraiDuracoes (com um com duraçao infinita) " ~: [Finita 5.0, Infinita] ~=?[Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }] ,
        "somaDuracao (com duas finitas)" ~: Finita 25 ~=?  (Finita 5) (Finita 20),
        "somaDuracao (com uma infinita)" ~: Infinita ~=? (Finita 10) Infinita,
        "verificaIguais (com diferentes)" ~: False ~=? [Fogo, Gelo, Fogo],
        "verificaIguais (com iguais)" ~: True ~=? [Gelo, Gelo, Gelo],
        "projeteisIguais (com 2 projeteis iguais)" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}] ~=? [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }],
        "projeteisIguais (com 2 diferentes)" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 8.0 }] ~=?  [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 4.0 }],
        "projeteisIguais (com resina)" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]
 ~=? [Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 5.0 }, Projetil { tipoProjetil = Gelo, duracaoProjetil = 3 }, Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }],
        "ativiaInimigo (com 3 inimigos na onda do portal)" ~: (Portal {posicaoPortal = (0, 0.5), ondasPortal = []}, [inimigo1,inimigo2]) ~=? Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [inimigo3, inimigo4, inimigo5]} : os)} [inimigo1, inimigo2], 
        "ativiaInimigo (sem ondas no portal)" ~: (Portal {posicaoPortal = (0, 0.5), ondasPortal = onda {inimigosOnda = is} : os}, inimigosjogo) ~=? Portal {posicaoPortal = (0, 0.5), ondasPortal = (onda@Onda {inimigosOnda = [] } : os)} inimigosjogo, 
        "ativiaInimigo (sem inimigos na onda do portal)" ~: (Portal {posicaoPortal = (0, 0.5), ondasPortal = []}, [inimigo1,inimigo2]) ~=? Portal {posicaoPortal = (0, 0.5), ondasPortal = []} [inimigo1,inimigo2], 
        "terminouJogo (derrota)" ~: True ~=? terminouJogo Jogo {baseJogo = Base {vidaBase = 0}, inimigosJogo = [Inimigo {posicaoInimigo = (2.5,2.5), vidaInimigo = 25}]}, 
        "terminouJogo (vitoria)" ~: True ~=? terminouJogo Jogo {baseJogo = Base {vidaBase = 15}, inimigosJogo = [] }           
        ]

