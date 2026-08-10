# Immutable Towers - Auditoria Técnica e Documental - 2026-07-28

Tags: #auditoria #qa #godot #migracao #prioridade

Relacionadas: [[00-Inicio/estado-atual|Estado Atual]], [[02-Planeamento/plano-proximas-fases|Plano das Próximas Fases]], [[04-Migracao-Godot/matriz-paridade|Matriz de Paridade]], [[04-Migracao-Godot/checklist-cutover|Checklist de Cutover]]

## Objetivo

Confirmar o estado real do projeto no código, nos testes, nos bundles e no vault antes de escolher o próximo trabalho. Esta auditoria não assume que uma funcionalidade documentada como concluída está pronta para release: distingue presença, cobertura automática, validação manual e qualidade de produto.

## Âmbito inspecionado

- estado Git e histórico recente;
- projeto Haskell, módulos novos e suite de testes;
- projeto Godot, cenas, domínio, persistência, contas, loja, bot e ferramentas;
- fixtures e exportador Haskell -> JSON;
- bundle Windows existente;
- todas as notas vivas do vault e respetivos links;
- hotspots de tamanho, tarefas abertas e inconsistências entre documentação e código.

## Evidência confirmada nesta auditoria

| Verificação | Resultado |
|---|---|
| `cabal build all` | passou |
| `cabal test --test-show-details=direct` | 166/166, zero erros e zero falhas |
| Godot 4.7 headless | 150/150 checks, sem erros de engine |
| Wikilinks do vault | 0 links partidos e 0 nomes duplicados |
| Bundle Godot | ZIP com 84 033 418 bytes e SHA-256 `77116B49E8078EC24120820E6693245C29837284C76C8FFDBF7FA162EA4A274D` |
| Benchmark 600 inimigos/120 torres | após grelha global, três execuções: média 5,19-5,40 ms; p95 6,72-7,30 ms; máximo 38,7-42,8 ms |

O benchmark confirma bom desempenho estabilizado, mas o pico inicial continua real e deve ser medido separadamente do p95. Não é atualmente um bloqueador funcional.

## Estado arquitetural confirmado

### Haskell

O Haskell continua compilável e é a referência de domínio, fixtures e rollback. As alterações locais acrescentam identidade de torres, inimigos, ondas, UI, fundações de contas/loja e a primeira estratégia de bot. O fluxo principal Haskell ainda usa o perfil global; as contas Haskell não foram integradas no arranque.

### Godot

O Godot já é um candidato jogável completo, não apenas um scaffold:

- nove torres e onze inimigos;
- cinco mapas e cinco modos;
- combate, ondas, bosses, economia, upgrades e especializações;
- contas locais, progressão, ranking, loja, recompensa pendente e fusão;
- partida pendente por conta e importação de saves Haskell;
- bot automático e sugestões;
- editor de mapas, opções e bundle Windows portátil.

A versão Godot ainda não deve substituir o launcher principal. Existem bloqueadores de integridade e validações manuais por fechar.

## Descobertas de prioridade crítica

### 1. O trabalho ainda não tem checkpoint Git

O `HEAD` continua em `069e82c`, anterior à migração. A auditoria encontrou alterações Haskell, reorganização completa do vault e 135 ficheiros não versionados apenas entre `docs/`, `godot/`, `migration/` e `tools/`.

Risco:

- perder uma migração funcional;
- misturar alterações antigas, migração e documentação num único diff difícil de rever;
- não existir um ponto de rollback reproduzível;
- futuros agentes assumirem que ficheiros não versionados pertencem ao baseline.

Decisão: criar um checkpoint revisto é a primeira ação da próxima fase. Nenhuma limpeza destrutiva deve ser feita antes dessa revisão.

### 2. Exportar uma conta Godot não permite importá-la de volta

`AccountRepository.export_account` escreve o schema `immutable-towers-account-export`. O único fluxo de importação da UI chama `import_transfer`, cujo validador aceita apenas `immutable-towers-transfer` com origem `haskell-gloss`.

Consequência: o botão `EXPORTAR CONTA` cria um ficheiro válido, mas esse ficheiro não pode atualmente ser restaurado pelo próprio jogo. Isto bloqueia backup real, mudança de computador e recuperação após eliminação.

Correção exigida:

- suportar importação do envelope de conta Godot ou unificar ambos os envelopes;
- definir política explícita para colisões de `AccountId`;
- testar exportar -> armazenamento limpo -> importar -> comparar todos os campos;
- incluir partida pendente, recompensa pendente, coleção, ranking e opções no round-trip.

### 3. Ações automáticas do bot não fazem checkpoint imediato

Construir, melhorar, vender e colocar obstáculo pelo jogador chamam `_checkpoint_run`. `VerticalSliceBot.apply` é executado pelo automático sem checkpoint posterior.

Consequência: fechar o jogo antes da vaga seguinte pode perder compras/upgrades do bot, mesmo que o jogador espere autosave por conta.

Correção exigida: guardar após cada ação mutável bem-sucedida do bot e adicionar um teste de regressão.

## Descobertas de prioridade alta

### Contas não são autenticação online

O sistema atual é um seletor de perfis locais:

- cria ou carrega automaticamente `player-1`;
- entra automaticamente na última conta;
- não tem palavra-passe, PIN, servidor ou sincronização cloud;
- não possui um fluxo explícito de terminar sessão;
- persiste no mesmo computador e pode futuramente viajar por exportação/importação.

Isto satisfaz persistência local, mas não o significado habitual de “login numa conta em qualquer computador”. Um login online exige backend, autenticação, recuperação de conta, política de conflitos e segurança; deve ser um projeto separado e não uma promessa implícita na UI.

### A repetição do bot tem duas causas diferentes

1. Uma conta nova desbloqueia apenas `Sentinela`; nesse estado, o bot não pode escolher outra torre.
2. Mesmo com várias torres, a heurística favorece cobertura bruta e compara upgrades pelo dano total/custo, não pelo ganho marginal.

Limitações confirmadas:

- só reconhece ameaças `Enxame` e `Armadura`;
- ignora resistência direta/área, escudo, regeneração, boss, velocidade e posição concreta da ameaça;
- procura apenas células imediatamente adjacentes à rota;
- não penaliza cobertura redundante das torres já construídas;
- o termo de eficiência por preço tem peso quase nulo;
- upgrade usa dano atual/custo em vez de delta de DPS, controlo e alcance;
- “poupar” só acontece quando nenhuma ação é comprável;
- decide uma vez por segundo de simulação, logo atua quatro vezes mais depressa em tempo real a 4x;
- as cinco referências cruzadas Haskell são executadas com apenas `Sentinela` disponível;
- não existem cenários automáticos fortes para swarm, blindado, rápido, boss, poupança e diversidade.

Conclusão: existe paridade com o bot Haskell inicial, mas não existe ainda a qualidade estratégica pretendida.

### Linguagem de desenvolvimento ainda aparece no produto

O código e os recursos ainda contêm termos como `vertical slice`, `fatia vertical`, `scaffold` e “versão em migração”. Alguns são internos, mas outros chegam ao HUD, estado final ou README do bundle. Antes do cutover, o jogador não deve ver linguagem de protótipo.

### Cobertura visual ainda é estreita

As quatro resoluções validadas são todas 16:9. Faltam:

- 1366x768 e 1280x800;
- 21:9;
- redimensionamento durante a execução;
- escala do Windows acima de 100%;
- navegação completa por teclado;
- contraste e legibilidade durante vagas densas;
- teste prolongado com HUD/painel recolhidos.

## Dívida técnica relevante

- `vertical_slice_simulation.gd`: 630 linhas e coordenação de demasiadas regras;
- `vertical_slice_screen.gd`: 390 linhas e mistura de input, persistência, bot e HUD;
- `boot_screen.gd`: 342 linhas e todos os ecrãs de menu/loja/perfil num único controlador;
- `account_repository.gd`: 287 linhas e mistura de contas nativas com migração Haskell;
- todos os 150 checks Godot vivem num único `run_tests.gd` com 727 linhas;
- a UI de menu reutiliza um `RichTextLabel` e cinco botões genéricos para páginas muito diferentes, limitando loja, coleção, perfil e ranking;
- o jogo ainda não possui áudio real, localização nem pipeline de assets licenciados;
- o tema define poucos tokens e estados visuais;
- os modelos continuam procedurais e legíveis, mas ainda parecem placeholders.

Esta dívida não exige uma reescrita. Deve ser extraída por componentes enquanto se acrescentam testes.

## Inconsistências encontradas no vault

- `estado-atual` misturava o estado Haskell das contas com o estado Godot já integrado;
- `sistema-contas`, `sistema-loja` e `sistema-progressao` descreviam sobretudo a fundação Haskell e estavam atrasados em relação ao Godot;
- a matriz marcava contas e save de partida como paridade automática sem registar o round-trip quebrado nem o checkpoint ausente do bot;
- o checklist de cutover deixava apenas a aprovação do launcher em aberto, apesar dos bloqueadores acima;
- `distribuicao` documentava apenas o bundle Haskell;
- os prompts em `90-Referencia` são históricos e não devem ser usados como roadmap atual;
- existem notas vazias/rascunhos (`2026-07-15`, `Sem título*`) que devem ser revistos manualmente antes de eventual remoção.

## Decisão

Estado de cutover: **ADIAR**.

A direção recomendada é continuar no Godot, preservar o Haskell como referência/rollback e fechar primeiro integridade de dados, checkpoint Git e QA de conta. Depois, evoluir o bot e fazer a ronda de design visual/loja/modelos. A ordem completa está em [[02-Planeamento/plano-proximas-fases|Plano das Próximas Fases]].

## Resolução posterior em 2026-07-28

Esta secção preserva a auditoria original e regista o que foi corrigido depois:

- importação nativa Godot e round-trip completo implementados;
- colisões permitem cancelar, importar como nova ou substituir explicitamente;
- IDs de conta passaram a usar bytes aleatórios de `Crypto`;
- onboarding, terminar sessão e lembrar conta foram ligados à UI;
- autosave cobre jogador, bot, vagas, pausa, menu, foco e fecho;
- confirmação de nova partida protege a partida pendente;
- linguagem de protótipo foi removida dos textos visíveis;
- Theme ganhou estados completos e a loja recebeu cards, custos, pools e requisitos;
- Bot 2.0 passou a considerar defesas, controlo, cobertura marginal, redundância, ganho marginal e poupança;
- suite Godot expandida primeiro para 176, depois 194, 200 e finalmente 211 checks;
- UI capturada também em 1366x768, 1280x800 e 2560x1080;
- rascunhos vazios `2026-07-15` e `Sem título*` foram removidos.
- capacidade estratégica implementada por modo, com compatibilidade para saves acima do limite e integração no bot/HUD;
- a primeira interpretação de footprint 3x3 foi corrigida após clarificação: cada torre ocupa uma célula global e todo o mundo foi escalado em conjunto;
- mapas-fonte 36x34 são agregados no Godot para 12x11 células de 66 px; terreno, estrada, torres, inimigos, portal, base e obstáculos usam a mesma escala;
- runs e mapas personalizados antigos são migrados pela marca `world_grid_scale`, com política conservadora que não elimina torres;
- loja recebeu componentes próprios, três modelos de baú, abertura e revelação persistente;
- fundo do menu recebeu uma batalha procedural em camadas;
- menu e loja mantêm a validação anterior; editor e nova grelha global foram revalidados numa janela real a 1280x720 e 1920x1080.

Continuam abertos: checkpoint Git sob pedido explícito, QA num segundo computador, resize/escala Windows, cenários completos do bot e extração gradual dos controladores grandes.
