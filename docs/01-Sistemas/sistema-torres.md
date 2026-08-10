# Immutable Towers - Sistema Torres

Tags: #sistema #torres

Relacionadas: [[HOME]], [[estado-atual]], [[backlog-jogo]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]]

## Modulos

### Haskell

- `app/TowerSystem.hs`: `TowerSpec`, loja, targeting, upgrades e balanceamento
- `app/TowerRuntime.hs`: identidade, nivel e especializacao por celula
- `app/Eventos.hs`: comprar, construir, melhorar, especializar e vender
- `app/Desenhar.hs`: modelos e painel contextual
- `app/SaveSystem.hs`: persistencia e migracao
- `lib/Tarefa3.hs`: combate generico com seletor e resolvedor injetados

### Godot

- `godot/data/domain-catalog-v1.json`: specs geradas pelo Haskell;
- `godot/src/domain/domain_catalog.gd`: validação e lookup;
- `godot/src/domain/vertical_slice_simulation.gd`: runtime e comandos;
- `godot/src/economy/tower_economy.gd`: preço, upgrade e venda;
- `godot/src/presentation/vertical_slice_view.gd`: modelos procedurais;
- `godot/src/presentation/vertical_slice_screen.gd`: arsenal e painel.

## Arquitetura

`LI12425.Torre` permanece compativel com a API academica. A aplicacao associa cada torre a um `TowerRuntime`:

- `TowerId`
- nivel atual
- especializacao opcional

A chave usa a celula inteira do mapa. `towerSpecDaTorre` consulta o registo explicito; `towerSpecAproximada` e apenas fallback para saves antigos e acoes legadas.

## Balanceamento base

| Torre | Papel | Alvo | Preco | Max |
|---|---|---:|---:|---:|
| Sentinela | dano consistente | primeiro | 44 | 4 |
| Glaciar | controlo | rapido | 62 | 5 |
| Braseiro | dano continuo | grupo | 74 | 5 |
| Panico | controlo de rota | primeiro | 92 | 5 |
| Venenoide | execucao | mais vida | 106 | 5 |
| Tesla | anti-enxame | grupo | 122 | 6 |
| Impacto | rajada pesada | mais vida | 138 | 6 |
| Solar | suporte ofensivo | primeiro | 146 | 6 |
| Tempestade | endgame | grupo | 188 | 7 |

## Capacidade de construção

O Godot limita o número de torres da partida por modo:

| Modo | Limite |
|---|---:|
| História | 18 |
| Infinito | 24 |
| Desafio | 12 |
| Bosses | 16 |
| Livre/Sandbox | 30 |

O limite é aplicado por `VerticalSliceSimulation`, portanto jogador e bot obedecem à mesma regra. `is_buildable` deixa de propor células quando a capacidade termina, vender liberta imediatamente uma vaga e o HUD mostra `usadas / limite`.

O Godot usa agora uma grelha global ampliada. O catálogo Haskell continua a guardar os mapas-fonte em 36x34, mas o runtime agrega cada bloco 3x3 antigo numa única célula grande:

- grelha jogável Godot: 12x11;
- tamanho visual da célula: 66 px, equivalente a três células antigas de 22 px;
- estrada, asfalto, água, relva, portal, base e obstáculos são convertidos em conjunto;
- cada torre volta a ocupar exatamente uma célula;
- clicar nessa célula seleciona a torre e o preview verde/vermelho cobre apenas essa unidade global;
- alcance, movimento e auras convertem as distâncias do catálogo por `1/3`, preservando aproximadamente a escala física e os tempos em vez de triplicar o alcance efetivo;
- o bot pesquisa e pontua apenas células globais legais.

`godot/src/domain/world_grid.gd` é a fonte de verdade da conversão. Caminho/asfalto têm prioridade dentro de cada bloco para não cortar rotas; água precisa de presença suficiente no bloco para substituir relva. Os cinco mapas oficiais são novamente validados depois da agregação.

Como a altura-fonte é 34, a conversão usa as primeiras 33 linhas em onze blocos completos; a última linha é apenas relva vazia nos cinco mapas oficiais e fica fora da área jogável. O tabuleiro passa de 792x748 px para 792x726 px, mantendo praticamente a mesma presença no ecrã.

A compatibilidade é conservadora: o snapshot grava `world_grid_scale = 3`; runs antigos sem a marca migram torres, inimigos e obstáculos para a célula global correspondente. Nenhuma torre é apagada se duas posições antigas convergirem para a mesma célula ou se a partida estiver acima do limite. Enquanto permanecer acima do limite, a partida só pode melhorar ou vender. Mapas personalizados 36x34 são migrados para o formato 12x11 ao carregar.

### Validação de densidade

Os cinco mapas têm 90-107 células de relva, mas só 30-47 ficam no alcance útil de uma Sentinela N1. Com limite 18 em História, só é possível ocupar cerca de 38-60% das posições úteis. Os alcances efetivos variam entre 1,03 e 1,80 células globais.

Decisão: manter grelha, escala e limites até existir telemetria humana. A facilidade atual vem sobretudo da eficiência da Sentinela, das vagas repetidas e da curva de campanha.

## Integridade económica

`TowerState` guarda separadamente `purchase_price_paid`, `upgrade_investment_paid` e `investment_paid`. A construção regista o custo ajustado ao modo, cada upgrade acrescenta apenas o valor efetivamente debitado e a venda devolve `floor(investimento * 0,65)`.

Saves antigos são migrados com uma estimativa conservadora: o preço de compra nunca excede o custo atual do modo e os upgrades são reconstituídos nível a nível. O painel mostra investimento e valor de venda.

A suite percorre nove torres, cinco modos, níveis, duas especializações e save/load. O gate acrescenta 45 transações completas e rejeita qualquer caso em que o saldo final exceda o inicial.

## Evidência de balanceamento

- Sentinela, Braseiro e Solar conseguem vencer História isoladamente no harness;
- Glaciar mantém a base viva, mas pode não terminar;
- Pânico perde cedo e também pode prender bosses;
- Venenoide, Tesla e Impacto isolados perdem nas vagas 2-3;
- Tempestade custa mais do que os créditos iniciais e pressupõe uma defesa anterior;
- o bot completo usa sobretudo Sentinela e consegue resultados piores do que Sentinela-only.

Uma torre de suporte não precisa vencer sozinha. A matriz serve para medir perfil, progresso e composição, e confirma que a Sentinela está demasiado perto de ser uma resposta universal.

## Especializacoes

Ao chegar ao ponto de escolha, o upgrade normal para ate o jogador selecionar:

- `DANO+`: mais 12% de dano e 0.18 de alcance nos upgrades seguintes, custo superior;
- `RAPIDA`: mais um alvo por rajada e ciclo 14% menor, com limite de seguranca.

A escolha e permanente, tem modelo visual proprio e e guardada no save.

## Targeting

- Primeiro na rota: menor distancia estimada a base.
- Rapido: maior velocidade efetiva.
- Mais vida: maior vida atual.
- Grupo: alvo mais proximo do centro dos candidatos.

A spatial grid limita primeiro os candidatos ao alcance; a ordenacao ocorre apenas nesse conjunto.

## Apresentação Godot

- nove silhuetas procedurais diferentes;
- cor derivada do catálogo;
- escala e anel extra nos níveis altos;
- alcance da torre selecionada;
- feedback de construção, upgrade, disparo e venda;
- painel com nível, dano, alcance e preview;
- especializações Potência/Cadência.

O modelo ainda comunica sobretudo por forma e cor. Especializações e níveis altos precisam de peças estruturais, animação e efeitos próprios, não apenas escala/anel.

## Aberto

- playtest de preco/DPS no inicio, meio e late game
- telemetria por torre para balanceamento
- efeitos/model swaps mais fortes nos upgrades altos
- cards de arsenal com ícone, papel, custo e estado
- diferença visual inequívoca entre especializações
- DPS efetivo contra resistências no painel e no bot
- manter temporariamente os limites 12/16/18/24/30 e ajustar apenas por telemetria humana
- garantir para cada torre um cenário em que é a melhor escolha e outro em que é fraca

Ver [[02-Planeamento/plano-melhoria-total-2026-07-29#4.4 Torres|Torres]] e [[02-Planeamento/plano-melhoria-total-2026-07-29#6.1 Gramática visual das torres|Modelos de torres]].
