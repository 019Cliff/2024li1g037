# Immutable Towers - Pontuacao

Tags: #sistema #gameplay #progressao #score

## Objetivo

Premiar progresso e execução eficiente sem recompensar espera, compra/venda repetida ou spam de torres.

## Implementacao Godot

`godot/src/gameplay/run_score.gd` é um serviço puro: recebe o estado final da simulação e as gemas de recompensa e devolve um inteiro determinístico.

A fórmula combina:

- vagas concluídas e inimigos eliminados;
- percentagem de vida restante da base;
- bónus de tempo que apenas diminui depois do tempo de referência do modo;
- eficiência de créditos com teto;
- multiplicador do modo.

Quantidade de torres e tempo decorrido positivo não dão pontos por si próprios. Construir uma torre que não participa na defesa não aumenta o score.

## Invariantes

- esperar nunca aumenta a pontuação;
- comprar e vender repetidamente não cria score;
- uma torre sem utilidade não cria score;
- os mesmos dados produzem sempre o mesmo inteiro;
- os dados necessários sobrevivem a save/load.

## Testes

A suite cobre idle, torre não utilizada, determinismo e round-trip de snapshot. O serviço fica separado de UI, ranking e persistência para permitir ajustar pesos sem alterar esses sistemas.

## Aberto

- calibrar tempos de referência por mapa, modo e perfil de dificuldade;
- medir distribuição de scores em campanhas humanas;
- mostrar breakdown opcional no ecrã de resultado;
- versionar a fórmula se rankings de versões diferentes precisarem de coexistir.

Relacionadas: [[sistema-progressao]], [[sistema-ondas]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].
