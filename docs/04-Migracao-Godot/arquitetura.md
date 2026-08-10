# Immutable Towers - Arquitetura da Migração Godot

Tags: #migracao #godot #arquitetura

Relacionadas: [[Indice-Migracao-Godot]], [[matriz-paridade]], [[contrato-saves]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]

## Camadas

1. `src/domain`, `src/combat` e `src/economy`: dados e regras puras, sem Nodes.
2. `src/gameplay` e `src/persistence`: aplicacao, comandos e armazenamento.
3. `src/presentation` e `scenes`: Nodes, UI, animacao e input.

O estado da partida sera uma unica fonte de verdade. Jogador e bot devem emitir os mesmos comandos de construir, melhorar, especializar e vender.

## Mapeamento

| Haskell                       | Godot                                   | Regra                                                  |
| ----------------------------- | --------------------------------------- | ------------------------------------------------------ |
| `LI12425`                     | DTOs de dominio                         | preservar campos, unidades e invariantes               |
| `Tarefa1`                     | validacao/rota                          | fixtures validas e invalidas                           |
| `Tarefa2`                     | resolucao de efeitos                    | mesmas combinacoes e duracoes                          |
| `Tarefa3`                     | simulacao fixa                          | movimento, disparo, morte e base                       |
| `EnemySpatial`                | indice espacial                         | sem scans globais no hot path                          |
| `MapData`                     | dados + `WorldGrid` + canvas 2D batched | IDs e mapa-fonte preservados; geometria Godot agregada |
| `TowerSystem`/`TowerRuntime`  | specs + runtime                         | identidade explicita por celula                        |
| `EnemySystem`                 | specs + runtime                         | resistencias, escudos e bosses                         |
| `WaveSystem`                  | DTOs declarativos                       | composicoes e mutadores                                |
| `BotStrategy`                 | dominio puro                            | primeiro paridade, depois melhorias                    |
| `ShopSystem`                  | transacoes puras                        | saldo nunca negativo                                   |
| `SaveSystem`/`AccountStorage` | JSON em `user://`                       | versionado, `.tmp` e `.bak`                            |
| `Desenhar`/`UI*`              | cenas, Containers e Theme               | preservar informacao, melhorar layout                  |

## Decisoes iniciais

- Godot 4.7 stable fixo durante a migracao.
- GDScript tipado.
- Compatibility renderer.
- celulas em `Vector2i`; centro visual convertido num unico servico.
- IDs persistidos em ASCII minusculo, nunca nomes traduzidos.
- placeholders procedurais ate existir licenca clara para assets.
- o limite Haskell/Godot usa JSON versionado; GDScript nunca interpreta `Show/Read`.
- `migration/fixtures` e a fonte neutra para casos de paridade consumidos pelos dois motores.

## Implementacao atual

- `tools/save-exporter/src/CatalogExport.hs` e a fonte do catalogo Godot; nao se duplicam specs manualmente.
- `DomainCatalog` valida e indexa o JSON uma vez no arranque.
- `VerticalSliceSimulation` mantem o estado deterministico da partida e nao depende de Nodes.
- `EnemyScaling` conserva a aritmetica Float32 do Haskell; `InfiniteWaveGenerator` gera mutadores sem depender da cena.
- `TowerEconomy` concentra preco por modo, custo de upgrade e venda.
- `CombatResolver`, `ProjectileEffects` e `EnemyRuntime` separam impacto, sinergias, efeitos temporais, movimento e chegada a base.
- `EnemySpatialHash` e reconstruido uma vez por update e reutilizado por targeting e auras.
- `MapPathfinder` calcula uma rota ordenada nas celulas `path` e `asphalt`.
- `AccountRepository` e `AtomicJsonStore` isolam contas e checkpoints em `user://` com `.tmp` e `.bak`.
- `WorldGrid` agrega cada bloco 3x3 do mapa-fonte 36x34 numa grelha Godot 12x11 e converte endpoints, mapas personalizados e coordenadas antigas.
- `GameLayout` centraliza a célula global de 66 px; alcance, auras e movimento convertem distâncias de catálogo por `1/3`.
- `LegacyRunConverter` converte uma partida exportada pelo Haskell para o snapshot Godot normal e remapeia posições para células globais.
- a UI e o bot chamam `build_tower`, `upgrade_tower` e `sell_tower`; nao possuem regras economicas paralelas.
- a capacidade de torres pertence a `VerticalSliceSimulation`, pelo que jogador, bot e restore usam a mesma política.
- cada torre ocupa uma célula global; validação e seleção pertencem à simulação e a apresentação desenha a mesma unidade.

O canvas batched foi mantido em vez de `TileMapLayer`: para uma grelha fixa 12x11 reduz Nodes e draw overhead, e os testes provam a rota depois da conversão dos mesmos dados-fonte. `VerticalSliceSimulation` ficou como coordenador de ondas, comandos e estado, delegando as regras de maior detalhe.

## Hotspots confirmados em 2026-07-28

| Ficheiro | Linhas | Responsabilidade a extrair quando necessário |
|---|---:|---|
| `vertical_slice_simulation.gd` | ~670 | coordenação de vagas, comandos, célula/capacidade, snapshot e runtime |
| `vertical_slice_screen.gd` | 457 | input, HUD, persistência e bot |
| `boot_screen.gd` | 492 | navegação, loja, perfil, ranking e contas |
| `account_repository.gd` | 403 | contas nativas, export e migração Haskell |
| `tests/run_tests.gd` | 955 | todos os domínios de teste |

Não fazer uma reescrita total. Extrair componentes por fatias verificáveis:

1. separar coordenação de ondas/snapshot de `VerticalSliceSimulation` quando uma alteração o exigir;
2. separar input/HUD de `vertical_slice_screen`;
3. extrair perfil, ranking e futura grelha de coleção de `boot_screen`; os visuais da loja já são componentes próprios;
4. dividir suites de teste por domínio com runner agregado.

## Invariantes adicionais

- ações do jogador e do bot continuam a usar a mesma API;
- apresentação nunca altera economia diretamente;
- qualquer transação persistente tem resultado e erro explícitos;
- importação nunca modifica a origem;
- colisões de conta exigem decisão explícita;
- velocidade visual 1x/2x/4x não altera a estratégia para o mesmo estado;
- novas torres exigem uma célula global de relva livre; restore migra coordenadas antigas e nunca apaga torres que convergem ou excedem o limite;
- termos internos como vertical slice não chegam ao texto final do produto.

Plano de execução: [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].
