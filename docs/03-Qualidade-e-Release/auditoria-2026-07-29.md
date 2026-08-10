# Immutable Towers — Auditoria técnica, de gameplay e visual — 2026-07-29

Tags: #auditoria #qa #godot #gameplay #bot #balanceamento #ui #release

Relacionadas: [[00-Inicio/estado-atual|Estado Atual]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]], [[02-Planeamento/backlog-jogo|Backlog]], [[04-Migracao-Godot/checklist-cutover|Checklist de Cutover]]

## Veredito executivo

A mudança para a grelha global está correta: o jogo usa agora um tabuleiro 12x11 em que cada célula representa um bloco 3x3 antigo, e torres, terreno, estrada, água, inimigos, portal, base e obstáculos partilham a mesma escala.

O candidato continua tecnicamente estável e rápido, mas os testes prolongados descobriram problemas de gameplay que os 211 checks existentes não detetam. O cutover deve continuar em **ADIAR**.

| Área | Estado | Decisão |
|---|---|---|
| Escala global 3x | Aprovada | Manter 12x11 e uma célula por torre |
| Testes automáticos | Aprovados | 211/211 Godot e 166/166 Haskell |
| Importação do projeto | Aprovada | Sem erros de scripts/recursos |
| Desempenho da simulação | Aprovado | p95 abaixo de 5,4 ms no stress test |
| Contas e save local | Aprovado neste computador | Falta segundo computador físico |
| Economia de compra/venda | **Bloqueador** | Existe lucro infinito em Desafio e Livre |
| Controlo e duração das vagas | **Bloqueador** | Gelo/medo conseguem criar encontros de 15+ minutos |
| Bot estratégico | **Bloqueador funcional** | Arsenal completo joga pior que apenas Sentinela |
| Desafio/Bosses | **Não validados** | Derrotas e stalemates sistemáticos no harness |
| UI responsiva | Parcial | Boa em 16:9; pequena a 1024 e desalinhada em 21:9 |
| Loja/modelos | Parcial | Componentes existem, mas ainda falta hierarquia e detalhe |
| Pacote Windows | **Não é release final** | Arranca, mas usa executável/editor com título `(DEBUG)` |

## Evidência executada

### Suites e importação

- `godot/run-tests.ps1`: **PASS, 211 checks**;
- `cabal test all`: **PASS, 166 casos**;
- importação headless do projeto no editor Godot 4.7: **PASS**, sem erros;
- o novo harness `godot/tools/audit_gameplay.gd` percorre mapas, modos, arsenais, economia e partidas prolongadas.

Os testes existentes continuam úteis para invariantes locais. A principal lacuna era não existir uma camada de testes de resultado: terminar partidas, detetar stalemates, medir diversidade do bot e validar que uma transação completa não cria dinheiro.

### Repetir o harness

A partir da pasta `godot`, com `GODOT_EXE` a apontar para o executável de consola:

```powershell
& $env:GODOT_EXE --headless --path . --script res://tools/audit_gameplay.gd
& $env:GODOT_EXE --headless --path . --script res://tools/audit_gameplay.gd -- focus
& $env:GODOT_EXE --headless --path . --script res://tools/audit_gameplay.gd -- economy
& $env:GODOT_EXE --headless --path . --script res://tools/audit_gameplay.gd -- transactions
```

O modo completo imprime mapas, campanhas do bot e perfis isolados. Os modos focados reduzem o tempo de diagnóstico. Nesta versão o harness é observacional e termina com sucesso mesmo quando encontra um resultado mau; a Fase 2 converte as invariantes críticas em exit code não-zero.

### Benchmark

Cenário sintético: 600 inimigos, 120 torres, 600 updates.

| Execução | Média | p95 | Máximo |
|---|---:|---:|---:|
| 1 | 4,87 ms | 5,39 ms | 37,55 ms |
| 2 | 4,87 ms | 5,01 ms | 45,77 ms |
| 3 | 4,70 ms | 4,67 ms | 36,60 ms |

Conclusão: a simulação estabilizada cabe confortavelmente no orçamento de 60 FPS. Os picos frios continuam acima de 16,7 ms e devem ser medidos com render e efeitos, mas não são o bloqueador atual.

### Pacote Windows

- ZIP: `release/ImmutableTowers-Godot-Windows.zip`;
- tamanho: **84 083 315 bytes**;
- SHA-256: `EA5965D5F02B43DA8AB84E7157BA5552E2CD1351BCCA3A4018A6640288BE215B`;
- extraído para uma pasta temporária isolada;
- arrancou sem Haskell, Cabal ou instalação de Godot;
- carregou a conta e a partida pendente atuais;
- o título da janela é **`Immutable Towers (DEBUG)`**.

O script atual exporta apenas o PCK e copia o executável completo do editor Godot. Isto serve como bundle portátil de desenvolvimento, mas não como release de produção. A documentação oficial do Godot recomenda um export com template otimizado e `--export-release`.

## Validação da escala e densidade dos mapas

### Métricas reais

| Mapa | Células de rota | Relva livre | Relva útil à Sentinela | Limite História |
|---|---:|---:|---:|---:|
| Planície Serena | 19 | 107 | 31 | 18 |
| Garganta de Pedra | 20 | 101 | 36 | 18 |
| Lago Fraturado | 23 | 94 | 35 | 18 |
| Cruzamento Solar | 28 | 94 | 46 | 18 |
| Bastião Espiral | 30 | 90 | 47 | 18 |

Embora existam 90–107 células de relva, só 30–47 ficam dentro do alcance útil de uma Sentinela de nível 1. O limite de 18 torres em História permite usar aproximadamente 38–60% dessas posições úteis, pelo que o jogador já não consegue preencher o mapa inteiro.

Os alcances efetivos na grelha global variam entre **1,03 e 1,80 células**. A melhor posição de uma torre de nível 1 cobre entre **7,1% e 26,3%** dos pontos da rota, dependendo de torre e mapa.

### Decisão

- não voltar ao footprint artificial 3x3 por torre;
- não reduzir novamente o mapa;
- manter temporariamente os limites 12/16/18/24/30;
- equilibrar primeiro ondas, torres, controlo e economia;
- só alterar limites depois de telemetria de partidas humanas.

O problema de facilidade restante não é a dimensão do mapa. A Sentinela barata, as ondas repetidas e a curva da campanha têm mais impacto.

## Descoberta crítica 1 — arbitragem de compra/venda

`build_tower` cobra o preço ajustado ao modo, mas `TowerState.purchase_price` guarda o preço base do catálogo. A venda calcula o reembolso a partir desse valor maior.

Transações lucrativas confirmadas:

| Modo | Torre | Compra | Venda imediata | Lucro |
|---|---|---:|---:|---:|
| Desafio | Sentinela | 32 | 40 | **+8** |
| Livre | Sentinela | 20 | 40 | **+20** |
| Livre | Glaciar | 38 | 49 | **+11** |
| Livre | Braseiro | 50 | 58 | **+8** |

No modo Desafio, o jogador pode repetir comprar → vender e gerar créditos infinitos.

### Correção exigida

1. guardar na torre o preço realmente pago;
2. calcular venda sobre investimento real, incluindo upgrades efetivamente pagos;
3. garantir `venda <= investimento acumulado` em todos os modos, níveis e especializações;
4. adicionar uma matriz automática de compra/upgrade/venda;
5. escolher uma taxa de reembolso explícita, por exemplo 60–70%, e não derivá-la indiretamente dos stats.

## Descoberta crítica 2 — controlo cria stalemates

O runtime atual:

- soma duração ao reaplicar efeitos;
- impede totalmente o movimento com gelo e eletricidade;
- faz o medo recuar enquanto estiver ativo;
- não aplica resistência crescente a crowd control;
- não tem regra anti-stalemate para vagas finitas.

Resultados após **900 segundos simulados**:

- Cruzamento Solar, História, arsenal completo: preso na vaga 4 com um Elite a 38,1% de vida;
- apenas Glaciar, História: preso na vaga 9 com dez inimigos ativos e base intacta;
- apenas Glaciar, Bosses: Ariete Veloz ainda ativo a 1% de vida;
- apenas Pânico, Bosses: Ariete Veloz ainda ativo a 81% de vida.

Uma partida que mantém a base viva, mas não consegue matar nem deixar avançar o último inimigo, não tem resolução.

### Correção exigida

- gelo deve abrandar fortemente ou aplicar um stun curto, não parar indefinidamente;
- hard CC deve usar duração máxima/cap, não soma ilimitada;
- bosses precisam de resistência, duração reduzida e/ou diminishing returns de controlo;
- medo em bosses deve ser convertido em slow/interrupt curto;
- adicionar progresso observável por encontro: dano, avanço na rota ou mudança de fase;
- criar watchdog de desenvolvimento que falha um teste quando uma vaga finita não progride durante um limite definido;
- criar um enrage/tenacidade de boss como proteção de produto, sem matar automaticamente o inimigo.

## Descoberta crítica 3 — o Bot 2.0 não melhora o resultado

### História, arsenal completo vs. apenas Sentinela

| Mapa | Arsenal completo | Só Sentinela |
|---|---|---|
| Planície Serena | vitória, 50,1 de vida, 608 s | vitória, 80 de vida, 528 s |
| Garganta de Pedra | vitória, 80 de vida, 581 s | vitória, 80 de vida, 506 s |
| Lago Fraturado | vitória, 50,1 de vida, 683 s | vitória, 80 de vida, 562 s |
| Cruzamento Solar | **não termina**, vaga 4 após 900 s | vitória, 80 de vida, 678 s |
| Bastião Espiral | vitória, 80 de vida, 843 s | vitória, 80 de vida, 797 s |

O bot avançado usou apenas **Sentinela, Glaciar e Solar**. Em quatro mapas construiu 15 Sentinelas em 18 posições. Não usou Braseiro, Pânico, Venenoide, Tesla, Impacto nem Tempestade.

Também tende a preencher o limite antes de melhorar: em História faz 18 construções e apenas seis upgrades; no Cruzamento constrói nove torres e passa 891 decisões a poupar.

### Causas confirmadas no código

- o score de construção continua a favorecer eficiência imediata da Sentinela;
- cobertura é binária por ponto da rota, sem tempo de contacto previsto;
- não existe simulação curta do resultado da vaga;
- o objetivo de poupança não verifica se existe rendimento futuro;
- pode poupar sem uma defesa capaz de matar, ficando sem forma de obter os créditos pretendidos;
- não existe regra de “defesa mínima antes de poupar”;
- não existe memória/histerese;
- a composição considera apenas a vaga visível/seguinte, não duas vagas completas;
- o auto bot perde a explicação detalhada ao executar `save`, porque “A poupar créditos” é tratado como erro/idle pelo runtime.

### Direção recomendada

O próximo bot deve ser um planeador pequeno e determinístico, não uma coleção maior de pesos:

1. gerar 5–12 ações relevantes;
2. simular cada ação durante uma janela curta e barata;
3. medir leaks, tempo para matar, créditos previstos e vida da base;
4. impor defesa mínima e impedir poupança sem fonte provável de rendimento;
5. manter objetivo até evento relevante;
6. escolher por resultado previsto e usar a heurística atual apenas como desempate;
7. registar decisão e resultado para calibrar pesos.

## Balanceamento de modos

### Desafio

Com 125 créditos, o bot de arsenal completo perde na vaga 2. A análise de sensibilidade deu:

| Créditos iniciais | Resultado do bot completo |
|---:|---|
| 125 | derrota na vaga 2 |
| 175 | derrota na vaga 2 |
| 225 | vitória com 15,8 de vida |
| 275 | vitória com 60 de vida |
| 350 | vitória com 43,7 de vida |

Isto não prova que um humano não consegue vencer com 125. Prova que o modo ainda não tem baseline automatizada aceitável e que 100 créditos mudam o resultado de forma abrupta.

### Bosses

O bot completo não terminou o modo com 180, 250, 350, 500 ou 700 créditos iniciais. Com mais créditos, a base muitas vezes fica viva mas o encontro estagna.

Primeiro deve corrigir-se o controlo. Só depois se ajustam vida, armadura, escudo, regen, escoltas, créditos e recompensas dos bosses.

### Torres isoladas em História

- Sentinela, Braseiro e Solar vencem;
- Glaciar chega à vaga 9, mas não termina em 900 segundos;
- Pânico perde na vaga 4;
- Venenoide, Tesla e Impacto perdem entre as vagas 2–3;
- Tempestade não pode ser comprada com os 150 créditos iniciais sem uma defesa anterior.

Uma torre de suporte não precisa vencer sozinha. A matriz mostra, contudo, que a Sentinela tem eficiência excessiva e que controlo/dano prolongado precisam de testes de composição, não só testes unitários por impacto.

## Campanha e pontuação

### Campanha sem curva real por estágio

Todos os mapas de História usam a mesma tabela de dez vagas. Capítulo e estágio alteram progressão/recompensa, mas não modificam as vagas.

As rotas crescem de 18 para 29 células. Mais rota dá mais tempo de ataque, pelo que mapas posteriores podem ficar mais fáceis mesmo tendo menos relva.

Recomendação:

- criar um `RunDifficultyProfile` derivado de capítulo, estágio, mapa e modo;
- variar vida, velocidade, composição, entrada e economia dentro de limites testáveis;
- dar identidade mecânica a cada mapa;
- evitar escalar apenas vida;
- definir uma faixa de vitórias alvo por estágio e conta.

### Pontuação recompensa demora

`_score` acrescenta `elapsed * 8` e `tower_count * 60`. Assim, demorar mais e encher o mapa com torres aumenta a pontuação.

Recomendação:

- pontos por vagas/ameaças eliminadas;
- bónus por vida da base;
- bónus de eficiência com teto;
- bónus de tempo decrescente, não crescente;
- multiplicador de modo/modificadores;
- zero incentivo a stalemate ou spam de torres.

## Contas e persistência

Validação real neste computador:

- conta JSON válida, schema/version corretos;
- progresso, coleção e gemas carregados;
- partida pendente com `world_grid_scale = 3`;
- ficheiro principal e `.bak` presentes;
- fechar e reabrir o ZIP recuperou conta e partida;
- perfil, menu, loja e save foram lidos sem erro.

O sistema continua a ser **local**. Não existe palavra-passe, backend ou cloud. O transporte entre computadores é feito por exportar/importar backup.

Melhorias pequenas:

- mostrar jogos/vitórias/derrotas como inteiros, não `7.0`, `4.0`, `3.0`;
- concluir playtest por UI de criar, trocar, exportar, importar e eliminar;
- validar num segundo computador;
- se for desejado login real em qualquer computador, criar um projeto separado de backend, autenticação, recuperação e conflitos.

## UI e arte

### O que funciona

- a grelha 12x11 está legível e coerente;
- torre, base, portal, inimigo e obstáculo têm escala comum;
- 1280x720 e 1920x1080 são utilizáveis;
- pausa e regresso ao menu funcionam;
- loja tem três modelos distintos, hover, flutuação, abertura e revelação;
- fundo do menu tem batalha procedural e respeita efeitos reduzidos.

### Problemas observados

- a 1024x768 o texto e os baús ficam demasiado pequenos;
- os cards da loja têm muito espaço vazio e o modelo do baú perde protagonismo;
- “GEMAS INSUFICIENTES” atravessa o modelo do baú;
- em 21:9 a partida fica ancorada à esquerda e sobra uma grande zona vazia à direita;
- em 4:3 a partida fica no topo e sobra espaço vazio abaixo;
- o gameplay usa offsets fixos em vez de um contentor centrado/responsivo;
- o fundo animado do menu é tão escuro e tapado pelos painéis que grande parte do detalhe não é percebida;
- níveis/especializações das torres continuam a mudar sobretudo escala e anel;
- inimigos partilham formas simples e estados dependem demasiado de cor/um anel.

Capturas locais desta auditoria:

- `godot/.godot/audit-menu-1024x768.png`;
- `godot/.godot/audit-shop-1024x768.png`;
- `godot/.godot/audit-menu-2560x1080.png`;
- `godot/.godot/audit-game-1024x768.png`;
- `godot/.godot/audit-game-1920x1080.png`;
- `godot/.godot/audit-game-2560x1080.png`.

## Arquitetura e manutenção

Hotspots atuais:

| Ficheiro | Linhas |
|---|---:|
| `godot/tests/run_tests.gd` | 1006 |
| `vertical_slice_simulation.gd` | 681 |
| `boot_screen.gd` | 492 |
| `vertical_slice_screen.gd` | 457 |
| `account_repository.gd` | 403 |
| `vertical_slice_bot.gd` | 366 |

Não é necessária uma reescrita. A extração deve seguir as próximas features:

- efeitos/controlo para um resolvedor próprio;
- regras de campanha/dificuldade para um perfil puro;
- score para um serviço puro;
- bot para contexto, geração de candidatos, avaliação e memória;
- gameplay screen para input, HUD e persistência;
- menu para loja, coleção, perfil e ranking;
- testes por domínio e harness de campanhas.

Não existe CI. Depois do checkpoint Git, criar uma pipeline que execute Haskell, Godot, import check, auditorias rápidas e build de release.

## Pesquisa externa aplicada

Referências usadas como princípios, não como lista de features a copiar:

- [Kingdom Rush 6 — Tower Level System](https://www.ironhidegames.com/News/Details/487): escolhas por torre, progressão sem grind e recompensa aspiracional;
- [Kingdom Rush 6 — modos Classic/Iron/Blitz](https://www.ironhidegames.com/News/Details/504): modos com restrições claras e identidade própria;
- [Kingdom Rush Alliance — Q&A](https://www.ironhidegames.com/News/Details/341): informação de inimigo acessível durante a batalha;
- [Isle of Arrows](https://store.steampowered.com/app/1946970/Isle_of_Arrows/?l=english): modificadores, campanhas com identidade e decisões espaciais;
- [Dungeon Warfare 3](https://www.excidiumgames.com/): dificuldade ajustável por modificadores/recompensa e conteúdo curado + infinito;
- [Godot — Multiple resolutions](https://docs.godotengine.org/en/stable/tutorials/rendering/multiple_resolutions.html): `canvas_items`, `expand`, anchors, escala de UI e suporte hiDPI;
- [Godot — Exporting projects](https://docs.godotengine.org/en/stable/tutorials/export/exporting_projects.html): templates e `--export-release`;
- [Xbox Accessibility Guidelines](https://learn.microsoft.com/en-us/xbox/accessibility/guidelines): texto, contraste, canais não cromáticos, navegação e redução de movimento.

## Decisão final

Não adicionar agora uma grande camada roguelite, heróis, multiplayer ou backend cloud.

Ordem correta:

`economia e stalemates → bot e modos → campanha/score → UI responsiva → modelos/loja/áudio → conteúdo opcional → release`

Plano de implementação e critérios de saída: [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].

## Resolução posterior em 2026-07-29

Esta secção preserva a auditoria original e regista o resultado da primeira execução do plano.

### Corrigido

- economia baseada no investimento realmente pago, com refund fixo de 65% e migração conservadora de saves antigos;
- matriz de 45 transações sem lucro em nove torres e cinco modos;
- política central de controlo com caps, velocidade mínima, tenacidade e diminishing returns;
- medo em bosses convertido em abrandamento para a frente;
- pontuação extraída para serviço puro, sem prémio por idle ou quantidade de torres;
- `poupar` tratado como decisão válida do bot, com rendimento provável e defesa mínima;
- perfil formatado com estatísticas inteiras;
- gate de gameplay integrado no runner local.

### Evidência automática

- Godot: **234/234 checks**;
- gate económico: **45/45 transações válidas**;
- gate de partidas: **9/9 partidas finitas resolvidas**, sem erro de ação e sem ausência prolongada de progresso;
- os cinco estágios perfilados do capítulo 1 vencem com 80, 80, 48,6, 47,8 e 80 de vida;
- História com arsenal completo em Cruzamento Solar: vitória, base com 50,1 de vida;
- História apenas com Glaciar: vitória, base com 7 de vida;
- dois cenários Boss terminam em derrota em cerca de 270-277 segundos, sem stalemate;
- Bot 3.0 usa rollout reduzido, memória e abertura económica; ainda fica abaixo de Sentinela-only no Cruzamento Solar;
- benchmark de 600 inimigos e 120 torres: média 5,36 ms, p95 5,95 ms por update.
- bundle de desenvolvimento reconstruído: 84 103 347 bytes, SHA-256 `E711BB64820666F8AA761552C704C07ABDCEC6638D45848ADD0BA6F32806FA4E`;
- o executável permaneceu ativo durante o smoke test de cinco segundos numa pasta temporária isolada.

### Ainda aberto

- provar que Bot 3.0 com arsenal completo supera Sentinela-only nos cinco mapas;
- equilibrar Desafio e Bosses para uma baseline de vitória aprovada;
- ampliar o harness com telemetria por torre e campanhas completas;
- corrigir reflow responsivo e validar escala Windows;
- instalar templates oficiais e gerar export Windows release sem `(DEBUG)`;
- checkpoint Git, CI e teste físico num segundo computador.

A decisão de cutover permanece **ADIAR**, agora sobretudo por bot, balanceamento, UI e release, não por integridade económica ou stalemates.
