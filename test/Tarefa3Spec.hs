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
      [ 
        "atualizajogo" ~: Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 200} ~=? atualizaJogo 5 [torre3] [inimigo4] portal2 base1,
        "atualizajogo" ~: Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 250} ~=? atualizaJogo 10 [torre3] [inimigo4, inimigo5] portal2 base1,
        "atualizaBase" ~: Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 150} ~=? atualizaBase 5 base1,
        "atualizaBase" ~: Base {vidaBase = 90.0, posicaoBase = (1.0,1.0), creditosBase = 150} ~=? atualizaBase 10 base1,
        "atingiuBase" ~: True ~=? atingiuBase base1 inimigo4,
        "atingiuBase" ~: False ~=? atingiuBase base1 inimigo5,
        "atualizaOnda" ~: Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0} ~=? atualizaOnda 5 [inimigo4] onda1 ,
        "atualizaOnda" ~: Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0} ~=? atualizaOnda 6 [inimigo5] onda1,
        "atualizaPortal" ~: Portal {posicaoPortal = (0.0,3.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 5.0},Onda {inimigosOnda = [Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]},Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}], cicloOnda = 2.0, tempoOnda = 3.0, entradaOnda = 0.0}]} ~=? atualizaPortal 5 [inimigo4] portal2,
        "atualizaTorre" ~: Torre {posicaoTorre = (0.0,1.0), danoTorre = 1.0, alcanceTorre = 2.0, rajadaTorre = 2, cicloTorre = 1.0, tempoTorre = -1.0, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}} ~=? atualizaTorre 5 [inimigo4] torre3,
        "atualizaInimigos" ~: [Inimigo {posicaoInimigo = (1.0,-4.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]}] ~=?  atualizaInimigos 5 [torre3] [inimigo4],
        "atualizaInimigo" ~: Inimigo {posicaoInimigo = (1.0,-0.25), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]} ~=? atualizaInimigo 5 [torre3] inimigo4,
        "atualizaProjeteis" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita (-5.0)}] ~=? atualizaProjetis 5 [projetil4],
        "aplicaEfeito" ~: Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.5, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]} ~=? aplicaEfeito projetil4 inimigo4,
        "moverInimigo" ~: (1.0,-4.0) ~=? moverInimigo 5 inimigo4,
        "calcularNovaPosicao" ~: (1.0,-4.0) ~=? calcularNovaPosicao inimigo4 5,
        "inimigoMorreu" ~: True ~=? inimigoMorreu inimigo4,
        "criarProjetil" ~: Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0} ~=? criarProjetil torre3 inimigo4,
        "criarProjetil" ~: Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0} ~=? criarProjetil torre3 inimigo5,
        "aplicaEfeitoProjetil" ~: Inimigo {posicaoInimigo = (1.0,1.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 0.25, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]} ~=? aplicaEfeitoProjetil inimigo4,
        "aplicaEfeitoProjetil" ~: Inimigo {posicaoInimigo = (4.0,1.0), direcaoInimigo = Sul, vidaInimigo = -2.0, velocidadeInimigo = 0.8, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]} ~=?  aplicaEfeitoProjetil inimigo5

      ]

instance Eq Jogo where
  (Jogo {baseJogo = base1, portaisJogo = p1, torresJogo = t1, mapaJogo = m1, inimigosJogo = i1, lojaJogo = l1})
    == (Jogo {baseJogo = base2, portaisJogo = p2, torresJogo = t2, mapaJogo = m2, inimigosJogo = i2, lojaJogo = l2}) =
    base1 == base2 && p1 == p2 && t1 == t2 && m1 == m2 && i1 == i2 && l1 == l2

instance Eq Base where
  (Base {posicaoBase = p1, vidaBase = v1})
    == (Base {posicaoBase = p2, vidaBase = v2}) =
    p1 == p2 && v1 == v2

instance Eq Torre where
  (Torre {posicaoTorre = p1, alcanceTorre = a1, rajadaTorre = r1, cicloTorre = c1, danoTorre = d1, tempoTorre = t1, projetilTorre = proj1})
    == (Torre {posicaoTorre = p2, alcanceTorre = a2, rajadaTorre = r2, cicloTorre = c2, danoTorre = d2, tempoTorre = t2, projetilTorre = proj2}) =
    p1 == p2 && a1 == a2 && r1 == r2 && c1 == c2 && d1 == d2 && t1 == t2 && proj1 == proj2

instance Eq Onda where
  (Onda {inimigosOnda = i1, cicloOnda = c1, tempoOnda = t1, entradaOnda = e1}) 
    == (Onda {inimigosOnda = i2, cicloOnda = c2, tempoOnda = t2, entradaOnda = e2}) =
    i1 == i2 && c1 == c2 && t1 == t2 && e1 == e2

instance Eq Portal where
  (Portal {posicaoPortal = p1, ondasPortal = o1}) 
    == (Portal {posicaoPortal = p2, ondasPortal = o2}) =
    p1 == p2 && o1 == o2

instance Eq Inimigo where
  (Inimigo {posicaoInimigo = p1, direcaoInimigo = d1, vidaInimigo = v1, velocidadeInimigo = vel1, ataqueInimigo = a1, butimInimigo = b1, projeteisInimigo = proj1}) 
    == (Inimigo {posicaoInimigo = p2, direcaoInimigo = d2, vidaInimigo = v2, velocidadeInimigo = vel2, ataqueInimigo = a2, butimInimigo = b2, projeteisInimigo = proj2}) =
    p1 == p2 && d1 == d2 && v1 == v2 && vel1 == vel2 && a1 == a2 && b1 == b2 && proj1 == proj2

instance Eq Projetil where
  (Projetil {tipoProjetil = t1, duracaoProjetil = d1}) 
    == (Projetil {tipoProjetil = t2, duracaoProjetil = d2}) =
    t1 == t2 && d1 == d2
