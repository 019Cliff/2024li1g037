# Immutable Towers - Sistema Inimigos

Tags: #sistema #inimigos

Relacionadas: [[HOME]], [[estado-atual]], [[sistema-ondas]], [[backlog-jogo]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]]

## Modulos

### Haskell

- `app/EnemySystem.hs`: classes, configuracao, resistencias, passivos e bosses
- `app/WaveSystem.hs`: composicao das vagas
- `lib/Tarefa3.hs`: update, movimento, spatial grid e injecao das regras da app
- `app/Desenhar.hs`: modelos, estados, barras e auras

### Godot

- `godot/data/domain-catalog-v1.json`: specs canónicas geradas;
- `godot/src/combat/combat_resolver.gd`: dano final;
- `godot/src/combat/projectile_effects.gd`: efeitos e sinergias;
- `godot/src/combat/enemy_runtime.gd`: estados temporais e chegada à base;
- `godot/src/gameplay/enemy_spatial_hash.gd`: procura espacial;
- `godot/src/domain/vertical_slice_simulation.gd`: coordenação;
- `godot/src/presentation/vertical_slice_view.gd`: modelos, barras, auras e feedback.

## Modelo compativel

O record academico `Inimigo` foi preservado. A classe e inferida por bandas estaveis de `velocidadeBaseInimigo`, enquanto vida, ataque e recompensa escalam por nivel. A velocidade efetiva e recalculada a partir da base antes dos efeitos, impedindo freezes permanentes.

## Classes

| Classe | Leitura | Counter principal |
|---|---|---|
| Basico | enxame regular | Sentinela/Braseiro |
| Rapido | pouca vida, muita velocidade | Glaciar |
| Tanque | vida e dano elevados | Impacto/Venenoide |
| Blindado | armadura e resistencia direta | dano prolongado/area |
| Regenerador | recupera vida | burst e foco de vida |
| Dispersor | resiste a area | single-target |
| Protegido | escudo acima de 66% de vida | quebrar escudo e focar |
| Elite | forte, rapido e com fase de velocidade | composicao mista |

## Bosses

- Ariete Veloz: acelera em duas fases de vida.
- Bastiao Vivo: armadura/escudo e aura que reduz o dano recebido por aliados proximos.
- Nexo da Ruptura: regenera, resiste a area e cria uma zona que reduz o dano das torres dentro dela.

As auras sao desenhadas com circulos pulsantes para comunicar alcance sem depender apenas da cor.

## Regras de dano

1. parte do dano e absorvida por armadura plana;
2. aplica-se resistencia direta ou de area conforme a torre;
3. o escudo pode reduzir o dano enquanto a vida esta alta;
4. auras de boss ajustam o dano final;
5. o projetil/estado da torre continua a ser aplicado.

## Performance

- torres consultam candidatos atraves de spatial grid, evitando comparar todas as torres com todos os inimigos
- classes e specs sao valores puros e pequenos
- o contexto de combate recolhe Guardioes e Rupturas uma vez por frame em O(n)
- cada impacto consulta apenas as posicoes dos bosses ativos, O(b), em vez de percorrer a vaga inteira

## Chegada a base

O movimento guarda a posicao inicial e verifica o segmento percorrido no frame contra a base. A celula da base tambem e uma zona terminal, para que a escolha de direcao nao possa inverter o inimigo na ultima celula do caminho. Se a velocidade ou o fast-forward fizerem o inimigo passar pela base sem ficar exatamente no centro dela, a posicao e ajustada para a base antes da separacao de estados. Assim o inimigo causa dano uma vez e nao pode inverter a rota por ter ultrapassado o alvo.

No Godot, velocidade de catálogo, alcance de auras e raio terminal são convertidos por `1/3` ao entrar na grelha global 12x11. A célula visual triplica, mas o tempo de percurso e a dimensão física aproximada das zonas permanecem comparáveis ao mapa-fonte.

Esta regra esta coberta por um teste de regressao com um inimigo a atravessar a base num unico frame.

## Controlo e stalemates

`crowd_control_policy.gd` centraliza limites e resistência. Reaplicar um estado refresca a duração até ao teto, sem a somar indefinidamente.

- gelo abranda com velocidade mínima de 25%/35%/50% para normal/elite/boss;
- eletricidade mantém stun curto com teto de 1,5/1,0/0,5 segundos;
- medo recua inimigos normais, mas em bosses converte-se num slow para a frente;
- hard CC repetido perde eficácia durante uma janela temporal;
- exposição e janela restante são guardadas no snapshot.

O gate mede dano, avanço na rota, mudança de vaga, inimigos pendentes e vida da base. Quatro partidas finitas terminam sem ausência prolongada de progresso; Glaciar e Pânico já não conseguem prender bosses indefinidamente.

## Aberto

- equilibrar vida/armadura dos bosses sobre a política de controlo estabilizada
- ampliar o gate a campanhas completas e validar 1x/2x/4x em máquinas reais
- IDs unicos apenas se forem necessarios para telemetria individual
- segunda fase **visual/telegraph** para o Nexo da Ruptura antes de alterar regras
- playtest dos counters e das auras em alta densidade
- silhuetas e animações mais distintas por classe
- feedback não cromático para armadura, escudo, regeneração e resistências
- cenários do Bot 3.0 para todas as defesas e bosses

Ver [[02-Planeamento/plano-melhoria-total-2026-07-29#1.2 Resolver controlo infinito e stalemates|Controlo]] e [[02-Planeamento/plano-melhoria-total-2026-07-29#4.5 Inimigos e bosses|Inimigos e bosses]].
