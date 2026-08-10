# Immutable Towers - Bot estrategico

Tags: #sistema #bot #ia #gameplay

Relacionadas: [[HOME]], [[estado-atual]], [[roadmap-atual]], [[backlog-jogo]], [[sistema-torres]], [[sistema-inimigos]], [[sistema-ondas]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]]

## Estado atual

- Haskell: `app/BotStrategy.hs` separa decisão e aplicação;
- Godot: `godot/src/bot/vertical_slice_bot.gd` compara construir, melhorar, especializar ou poupar;
- `godot/src/bot/bot_runtime.gd` aplica ações e força checkpoint quando existe mutação;
- jogador e bot usam os mesmos comandos de domínio;
- decisão determinística com score, razão e alternativas;
- o Godot considera inimigos ativos, pendentes e a vaga seguinte;
- cinco referências Haskell históricas continuam válidas como fixtures de leitura, mas posição e score já não coincidem: o Godot decide na grelha global 12x11 enquanto as referências usam coordenadas-fonte 36x34;
- o automático decide por tempo real, independentemente de 1x/2x/4x;
- Sandbox disponibiliza todo o catálogo ao bot; os restantes modos respeitam o arsenal desbloqueado.

## Resultado real da auditoria prolongada

O Bot 2.0 é legal e determinístico, mas ainda não é estrategicamente competente.

Em História, arsenal completo vs. apenas Sentinela:

| Mapa | Arsenal completo | Apenas Sentinela |
|---|---|---|
| Planície Serena | vitória, base 50,1, 608 s | vitória, base 80, 528 s |
| Garganta de Pedra | vitória, base 80, 581 s | vitória, base 80, 506 s |
| Lago Fraturado | vitória, base 50,1, 683 s | vitória, base 80, 562 s |
| Cruzamento Solar | não termina em 900 s | vitória, base 80, 678 s |
| Bastião Espiral | vitória, base 80, 843 s | vitória, base 80, 797 s |

Com o catálogo completo, o bot só usou Sentinela, Glaciar e Solar e chegou a construir 15 Sentinelas em 18 posições. Também tende a construir até ao limite antes de melhorar. No Cruzamento Solar constrói nove torres e depois passa centenas de decisões a poupar sem rendimento suficiente para chegar ao objetivo.

Estes resultados tornam a diversidade e o outcome gates funcionais, não apenas melhorias futuras.

## Porque pode colocar sempre a mesma torre

### Arsenal limitado

Uma conta nova desbloqueia apenas `Sentinela`, e a simulação recebe apenas as torres desbloqueadas. Nesse contexto o bot preserva a referência Haskell e explica na decisão que o arsenal só contém Sentinela.

### Heurística atual

Com várias torres, a decisão considera:

- armadura, resistência direta/área, escudo e regeneração;
- velocidade, pressão próxima da base e densidade do grupo;
- controlo por tipo de projétil;
- cobertura marginal e penalização de redundância;
- eficiência de custo;
- ganho marginal de DPS/alcance no upgrade;
- objetivo de poupança quando uma ação superior está próxima.

Ainda não existe memória de objetivo entre decisões, histerese, previsão por simulação nem telemetria local. A cobertura binária não representa tempo de contacto e a poupança não verifica defesa mínima nem rendimento futuro.

## Cobertura de testes

Coberto:

- determinismo básico;
- ação legal;
- construção apenas em células globais legais;
- todos os cinco mapas convertidos suportam as 30 torres do Sandbox;
- economia não negativa;
- leitura das cinco referências Haskell e divergência geométrica intencional;
- breakdown do score e limite de alternativas;
- checkpoint imediato após ação automática;
- primeiro comportamento estratégico com arsenal avançado.
- `poupar` preservado como decisão válida e explicável;
- poupança limitada a objetivos atingíveis por rendimento provável e com defesa mínima;
- prioridade de construção reduzida à medida que a capacidade satura;
- rollout reduzido sobre no máximo 12 candidatos, com dano, pressão, rendimento e reserva;
- planeador com objetivo, última ação e janela de cinco identidades recentes;
- abertura reserva créditos para três defesas antes de considerar compras caras;
- gate finito sem erros de ação ou partidas bloqueadas.

Insuficiente:

- as cinco referências Haskell usam apenas Sentinela e coordenadas da grelha-fonte 36x34;
- o gate reduzido já é executado pela suite local, mas ainda falta CI;
- faltam assertions fortes de diversidade e superioridade sobre Sentinela-only;
- faltam cenários dedicados por defesa, mutador e boss depois de corrigir controlo;
- falta uma baseline aprovada de construção vs. upgrade vs. poupança;
- falta benchmark específico do planeamento estratégico.

## Bot 3.0

A próxima versão não deve ser apenas uma nova tabela de pesos. Deve:

1. terminar a separação de contexto e geração de candidatos;
2. manter 5-12 ações relevantes;
3. calibrar o rollout curto determinístico já implementado;
4. medir leaks, tempo para matar, vida da base e rendimento;
5. rejeitar poupança sem defesa mínima ou fonte provável de créditos;
6. manter um objetivo até existir um evento que justifique mudar;
7. usar a heurística atual como filtro/desempate;
8. registar previsão e resultado para calibração.

O runtime preserva `poupar` como decisão planeada, sem erro nem checkpoint falso. O planeador mantém memória/histerese local e o harness regista as primeiras 20 ações de cada partida.

Na comparação atual, o arsenal completo:

- preserva 80 de vida em Planície, Garganta, Lago e Bastião;
- termina Lago e Bastião mais depressa do que Sentinela-only;
- termina Cruzamento mais depressa, mas deixa passar um inimigo e fica com 50,1 de vida;
- usa apenas uma ou duas identidades por mapa.

Por isso a aceitação de superioridade/diversidade continua aberta.

Aceitação: arsenal completo iguala ou supera Sentinela-only nos cinco mapas, termina Cruzamento Solar, usa pelo menos seis identidades na matriz completa e não poupa sem rendimento.

Plano e critérios completos: [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 3 - Bot 3.0|Fase 3 - Bot 3.0]].

## Desempenho

O mapa é percorrido para cada torre e a lista fica limitada a oito posições por torre. Na próxima versão, pré-calcular features estáticas por mapa e atualizar apenas ameaça/cobertura dinâmica. Rollouts precisam de estado reduzido, limite de candidatos e benchmark próprio. O benchmark deve continuar separado entre warmup, p95 estabilizado e pior frame.
