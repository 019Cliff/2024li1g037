module Tarefa2Spec (testesTarefa2) where

import Test.HUnit

import Tarefa2
import Tarefa1
import LI12425


testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [
        "inimigosNoAlcance" ~: [] ~=? inimigosNoAlcance torre2 [], 
        "inimigosNoAlcance" ~: [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}] ~=? inimigosNoAlcance torre2 [inimigo1],
        "atingeInimigo " ~: Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 99.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]}  ~=? atingeInimigo torre2 inimigo1,
        "fogoEGelo" ~: [Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}] ~=? fogoEGelo [projetil1, projetil2, projetil3],
        "fogoEGelo (com lista vazia)" ~:  []  ~=? fogoEGelo [],
        "encontraGelo" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}] ~=? encontraGelo [projetil2, projetil3, projetil2],
        "encontraGelo (com lista vazia)" ~: [] ~=? encontraGelo [],
        "atingeFogoEResina" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}] ~=? atingeFogoEResina [projetil1, projetil3, projetil2],
        "atingeFogoEResina" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}] ~=? atingeFogoEResina [projetil2],
        "atingeFogoEResina" ~: [] ~=? atingeFogoEResina [],
        "encontraFogo" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}] ~=? encontraFogo [projetil1, projetil2, projetil1],
        "encontraFogo (com lista vazia)" ~: [] ~=? encontraFogo [],
        "encontraResina" ~: [Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}] ~=? encontraResina [projetil3, projetil2, projetil3],
        "encontraResina (com lista vazia)" ~: [] ~=? encontraResina [],
        "duplicaDuracao" ~: Finita 10.0 ~=? duplicaDuracao (Finita 5.0),
        "duplicaDuracao (com duraçao infinita) " ~: Infinita ~=? duplicaDuracao Infinita,
        "dividePorTipoProjetil" ~: ([],[],[])  ~=? dividePorTipoProjetil [] ,
        "dividePorTipoProjetil" ~: ([Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}],[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}],[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}])~=? dividePorTipoProjetil [projetil1, projetil2, projetil3],
        "somaDuracoes Infinita" ~: Infinita ~=?  somaDuracoes [Finita 5.0, Infinita],
        "somaDuracoes (tres diferentes)" ~: Finita 12.0 ~=?  somaDuracoes [Finita 5.0, Finita 3.0, Finita 4.0],
        "verificaIguais (com iguais)" ~: True  ~=? verificaIguais [projetil1, projetil2, projetil1],
        "verificaIguais (com diferentes)" ~: False ~=? verificaIguais [projetil2, projetil3],
        "somaProjetil (fogo fogo gelo)" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}] ~=? somaProjetil [Projetil Fogo (Finita 5.0), Projetil Fogo (Finita 5.0), Projetil Gelo (Finita 3.0)],
        "somaProjetil Vazio" ~: [] ~=? somaProjetil [],
        "atualizaprojeteis" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}] ~=? atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}],
        "atualizaprojeteis" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}] ~=? atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}],
        "atualizaprojeteis" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}] ~=? atualizarProjetis (Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}],
        "atualizaprojeteis" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}] ~=? atualizarProjetis (Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}) [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}],
        "atualizaprojeteis" ~: [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}] ~=? atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}],
        "removeEfeito Gelo0" ~: Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Oeste, vidaInimigo = 80.0, velocidadeInimigo = 0.5, ataqueInimigo = 15.0, butimInimigo = 70, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}]} ~=? removeEfeito Gelo inimigo2,
        "removeEfeito Fogo" ~: Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []} ~=? removeEfeito Fogo inimigo1,
        "ativaInimigo" ~: (Portal {posicaoPortal = (0.0, 0.0), ondasPortal = []}, [inimigo1]) ~=? ativaInimigo portal1 [inimigo1],
        "ativiaInimigo (com 3 inimigos na onda do portal)" ~: (Portal {posicaoPortal = (0.0,0.0), ondasPortal = []},[]) ~=? ativaInimigo portal1 [], 
        "terminouJogo (derrota)" ~: True ~=? terminouJogo jogo1 {baseJogo = base1 {vidaBase = 0}},
        "terminouJogo (vitoria)" ~: False ~=? terminouJogo jogo1           
      ]
      

instance Eq Inimigo where
  (Inimigo {posicaoInimigo = p1, direcaoInimigo = d1, vidaInimigo = v1, velocidadeInimigo = vel1, ataqueInimigo = a1, butimInimigo = b1, projeteisInimigo = proj1}) 
    == (Inimigo {posicaoInimigo = p2, direcaoInimigo = d2, vidaInimigo = v2, velocidadeInimigo = vel2, ataqueInimigo = a2, butimInimigo = b2, projeteisInimigo = proj2}) =
    p1 == p2 && d1 == d2 && v1 == v2 && vel1 == vel2 && a1 == a2 && b1 == b2 && proj1 == proj2

instance Eq Projetil where
  (Projetil {tipoProjetil = t1, duracaoProjetil = d1}) 
    == (Projetil {tipoProjetil = t2, duracaoProjetil = d2}) =
    t1 == t2 && d1 == d2

instance Eq Portal where
  (Portal {posicaoPortal = p1, ondasPortal = o1}) 
    == (Portal {posicaoPortal = p2, ondasPortal = o2}) =
    p1 == p2 && o1 == o2

instance Eq Onda where
  (Onda inimigos1 ciclo1 tempo1 entrada1) == (Onda inimigos2 ciclo2 tempo2 entrada2) =
    inimigos1 == inimigos2 && ciclo1 == ciclo2 && tempo1 == tempo2 && entrada1 == entrada2
