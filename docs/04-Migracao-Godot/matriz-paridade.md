# Immutable Towers - Matriz de Paridade Haskell -> Godot

Tags: #migracao #paridade #godot

Relacionadas: [[checklist-cutover]], [[diario-migracao]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]

Estados: `nao iniciado`, `parcial`, `paridade automatica`, `paridade manual`, `diferenca aceite`.

Nota: paridade prova equivalência com a referência Haskell; não prova, por si só, qualidade final de produto, diversidade do bot ou prontidão de release.

| Sistema | Estado | Evidencia atual | Gate ainda aberto |
|---|---|---|---|
| Projeto/boot Godot | paridade automatica | Godot 4.7 abre, importa, instancia, executa 250 checks headless e o gate de gameplay | nenhum funcional |
| IDs e catalogo | paridade automatica | Haskell gera 9 torres, 11 inimigos, 5 mapas e 5 modos | manter geracao antes de releases |
| Mapas e rota | diferença aceite | cinco mapas-fonte 36x34 são agregados para 12x11; portal-base, asfalto/água, rotas e migração de mapa personalizado são testados; células globais usam 66 px | playtest visual dos cinco mapas |
| Combate | diferença aceite | dano preserva a referência; controlo usa caps, tenacidade e movimento mínimo próprios do Godot; nove partidas finitas passam o watchdog | ampliar gate a velocidades completas |
| Torres | diferença aceite | 98 estados de tier/especializacao, custos, venda e 45 precos por modo coincidem com Haskell; Godot usa uma célula global por torre, limites 18/24/12/16/30, restore migratório e contador | afinar grelha/limites por playtest |
| Inimigos | paridade automatica | onze classes atravessam fixtures de dano/defesa; efeitos, movimento, bosses, passivos e spatial hash testados | telegraph manual dos bosses |
| Ondas e modos | diferença aceite | História mantém a referência no estágio 1 e perfila os estágios 2-5; os cinco vencem na gate; Desafio/Boss/Sandbox e 18 vagas infinitas preservam a base Haskell; Infinito recompensa a vaga alcançada até 188 gemas | playtest late game 4x, curva de gemas e capítulos posteriores |
| Economia | diferença aceite | preço efetivamente pago e upgrades formam investimento explícito; refund de 65%; 45 transações e save/load sem arbitragem | playtest humano de clareza do painel |
| Bot estrategico | parcial | ações são legais/determinísticas, mas arsenal completo perde para Sentinela-only e pode poupar sem rendimento | Bot 3.0, campanhas por classe/boss, memória, rollouts e telemetria |
| Save/import Haskell | paridade automatica | save real copiado foi importado pela UI para `legacy-global`; SHA-256 da origem ficou inalterado; rollback por `.bak` testado | nenhum funcional |
| Contas locais | paridade automatica | onboarding, criar, selecionar, renomear, terminar sessão, lembrar conta e duas contas independentes testadas; dados usam `.tmp`/`.bak` | playtest após restart |
| Backup de conta Godot | paridade automatica | round-trip nativo preserva o documento normalizado; colisão permite cancelar, copiar ou substituir | validar transferência num segundo computador físico |
| Loja meta | paridade automatica | baus, seed, duplicados, recompensa pendente, colecao e fusao persistentes; três modelos, abertura e revelação visual testados | interrupção manual da abertura e futura grelha de coleção |
| Save de partida | paridade automatica | checkpoint por conta, bot, vaga, pausa, menu, foco e fecho; continuar/nova partida, mapa custom e formato antigo testados | fechar/reabrir manualmente o bundle |
| UI responsiva | parcial | 1280x720 e 1920x1080 utilizáveis; 1024 fica pequeno, 21:9 ancora à esquerda e 4:3 ao topo | contentores/reflow, escala de UI, resize e escala Windows |
| Editor de mapas | paridade automatica | validacao, save/load, portal/base, formato 12x11, migração 36x34 e jogar mapa personalizado testados | ergonomia manual |
| Obstaculos dinamicos | diferenca aceite | ate 6 bloqueios persistentes abrandam em O(1) sem destruir a rota; colocacao, rejeicao, movimento e restore testados | balanceamento manual |
| Bundle Windows | parcial | ZIP de desenvolvimento extraído e iniciado sem Haskell/Cabal/Godot; conta/save carregam | export release real sem `(DEBUG)` e segundo computador |

`Parcial` significa que o comportamento existe e tem cobertura, mas ainda falta uma fixture cruzada ou validacao manual indicada na ultima coluna.

Auditoria que justificou as alterações de estado: [[03-Qualidade-e-Release/auditoria-2026-07-29|2026-07-29]].
