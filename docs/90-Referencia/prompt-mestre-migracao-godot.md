# Prompt Mestre — Migração segura de Haskell/Gloss para Godot

Tags: #prompt #migracao #godot #arquitetura #preservacao #roadmap

Relacionadas: [[HOME]], [[estado-atual]], [[roadmap-atual]], [[backlog-jogo]], [[sistema-bot]], [[sistema-contas]], [[sistema-loja]], [[sistema-progressao]], [[sistema-ui]], [[sistema-torres]], [[sistema-inimigos]], [[sistema-ondas]]

> Alvo recomendado: GPT-5.6 Sol. O prompt foi escrito para uma tarefa longa, com ferramentas, validação contínua e resultados verificáveis.

> Estado: executado como migração lado a lado entre 2026-07-13 e 2026-07-15. Este prompt fica como histórico. Para continuar, usar [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]] e [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].

## Decisão de segurança

A migração é **lado a lado**, não uma reescrita destrutiva:

- o projeto Haskell permanece na raiz e continua compilável;
- o novo projeto fica exclusivamente em `godot/`;
- scripts, assets, documentação, saves e testes antigos não são apagados nem movidos;
- Haskell e Godot ficam executáveis em paralelo durante toda a migração;
- o arranque principal não muda para Godot antes de paridade e aprovação explícita;
- os saves antigos são importados por um contrato JSON versionado, mantendo os originais intactos.

## Como usar

Copiar a secção **Prompt** para uma nova tarefa no repositório. O agente deve trabalhar diretamente nos ficheiros e implementar por fases, não devolver apenas um plano.

## Prompt

Estás a migrar o jogo **Immutable Towers** de Haskell/Gloss para **Godot 4 com GDScript tipado**, preservando integralmente o projeto atual como versão funcional de referência. Trabalha como arquiteto de jogos, programador sénior, especialista em migrações e QA.

# Personalidade e colaboração

Sê direto, cuidadoso e persistente. Começa tarefas longas com uma atualização curta que diga o que vais verificar primeiro. Faz suposições razoáveis quando forem reversíveis, mas não assumes permissão para operações destrutivas, instalações globais, commits de alterações alheias ou perda de dados. Comunica riscos com evidência concreta e mantém o utilizador informado durante trabalho demorado.

# Objetivo

Criar dentro de `godot/` uma nova implementação do Immutable Towers que atinja paridade funcional e melhore a capacidade de produzir UI, animações, assets, partículas, áudio e conteúdo. O Haskell existente deve continuar a compilar e servir como especificação executável até ao fim.

A migração deve conservar:

- regras de combate, economia, progressão e targeting;
- cinco mapas, cinco modos, ondas e mutadores;
- nove torres, níveis, especializações, raridades e desbloqueios;
- classes de inimigos, resistências, bosses e efeitos;
- bot estratégico e o seu comportamento determinístico;
- contas locais, perfil, ranking, loja meta e recompensas;
- partidas guardadas, incluindo identidade e runtime das torres;
- editor de mapas, controlos e fluxos de menu relevantes;
- direção visual estabelecida no vault, melhorando-a através das ferramentas do Godot.

# Critérios de sucesso

A tarefa só está concluída quando:

1. todos os ficheiros Haskell originais continuam presentes e `cabal build all` passa;
2. toda a suite Haskell passa — 166 testes no baseline observado, mas usa o número exato encontrado no início;
3. existe um projeto Godot autónomo em `godot/project.godot` que abre sem erros na versão estável escolhida;
4. existem testes Godot headless para regras puras e fixtures de paridade com Haskell;
5. as funcionalidades assinaladas como migradas na matriz de paridade estão realmente jogáveis;
6. os saves Haskell podem ser exportados para um JSON versionado e importados no Godot sem apagar os originais;
7. uma importação repetida é idempotente e não substitui progresso Godot mais recente sem confirmação;
8. Haskell e Godot têm scripts de execução separados e continuam utilizáveis lado a lado;
9. existe um bundle Windows Godot verificável, sem remover o bundle Haskell;
10. o vault documenta diferenças, decisões, testes, limitações e instruções de rollback;
11. não existem afirmações de paridade visual ou funcional sem teste correspondente;
12. mudar o arranque padrão para Godot fica fora do âmbito até o utilizador aprovar explicitamente o cutover.

# Restrições não negociáveis

- Não uses `git reset --hard`, `git clean`, `git checkout --`, eliminações recursivas ou qualquer comando que descarte alterações.
- Não apagues, movas ou reescrevas em massa `app/`, `lib/`, `test/`, scripts Cabal ou documentação existente.
- Não faças commit, stash ou stage de alterações preexistentes sem autorização explícita.
- Não cries uma cópia inteira e divergente do repositório dentro dele; a preservação é feita por Git/checkpoint e pela coexistência de `godot/`.
- Não transformes `run-game.ps1`, `run-game.bat` ou o empacotamento Haskell no launcher Godot durante a migração.
- Não mudes balanceamento apenas para facilitar o port. Qualquer diferença intencional precisa de teste, justificação e registo.
- Não parses diretamente em GDScript o formato textual produzido por `Show/Read`. Cria um exportador Haskell explícito e um contrato JSON.
- Não apagues nem alteres os ficheiros de save antigos depois da importação.
- Não inventes autenticação online. As contas continuam locais; backend e cloud sync estão fora do âmbito.
- Não instales Godot, plugins, SDKs ou addons globalmente sem autorização. Se o executável não existir, cria o scaffold que seja verificável sem ele, regista o bloqueio e pede apenas a autorização mínima necessária para instalar/apontar uma versão portátil oficial.
- Não copies assets de origem ou licença incerta para a nova distribuição. Usa placeholders procedurais até a proveniência estar confirmada.
- Não uses addons pesados quando o motor já fornecer a capacidade necessária.
- Mantém nomes persistidos e IDs em formato estável; nunca uses texto traduzido ou posição numa enum como identificador de save.

# Baseline que tens de voltar a auditar

Lê antes de alterar:

- `README.md`, `immutable-towers.cabal`, scripts de execução/distribuição e todo o vault `docs/`;
- toda a API académica em `lib/`, especialmente `LI12425.hs`, `Tarefa1.hs`, `Tarefa2.hs`, `Tarefa3.hs`, `EnemySpatial.hs` e `MapGeometry.hs`;
- todos os sistemas em `app/`, com atenção a `ImmutableTowers.hs`, `GameFactory.hs`, `MapData.hs`, `MapEditor.hs`, `TowerSystem.hs`, `TowerRuntime.hs`, `EnemySystem.hs`, `WaveSystem.hs`, `BotStrategy.hs`, `BotSystem.hs`, `ProgressionSystem.hs`, `ShopSystem.hs`, `AccountTypes.hs`, `AccountSystem.hs`, `AccountStorage.hs`, `SaveSystem.hs`, `Eventos.hs`, `Tempo.hs`, `Desenhar.hs`, `UIState.hs`, `UIRects.hs`, `VisualTheme.hs` e `UIIcons.hs`;
- toda a suite em `test/`, incluindo `BotStrategySpec.hs` e `AccountSystemSpec.hs`;
- todos os assets atuais e qualquer informação de licença disponível.

Confirma no código, em vez de confiar apenas nesta fotografia:

- o projeto usa espaço virtual 1920x1080; o catálogo conserva mapas-fonte 36x34 e o candidato Godot agrega-os numa grelha jogável 12x11 de células globais;
- terrenos: Relva, Terra, Asfalto e Agua;
- mapas: PlanicieSerena, GargantaPedra, LagoFraturado, CruzamentoSolar e BastiaoEspiral;
- modos: Historia, Infinito, Desafio, Boss e Sandbox;
- torres: Sentinela, Glaciar, Braseiro, Panico, Venenoide, Tesla, Impacto, Solar e Tempestade;
- `TowerRegistry` conserva `TowerId`, nível e especialização por célula;
- inimigos incluem oito classes normais e três bosses;
- `BotStrategy` já compara construções/posições/upgrades e devolve score, razão e alternativas, mas ainda recebe apenas `Jogo` e não domina todo o runtime;
- contas locais têm tipos e armazenamento versionado, mas o fluxo principal ainda usa os saves globais;
- `SaveSystem` usa `GameSaveV2` e fallback para um `Jogo` legado;
- os ficheiros globais atuais são `immutable-towers-meta.txt` e `immutable-towers-save.txt`;
- o armazenamento de contas usa a pasta de dados da aplicação `ImmutableTowers`, `accounts-index-v1.txt`, `profile-v1.txt`, `.tmp` e `.bak`;
- a loja meta já tem domínio puro e cartões clicáveis, mas recompensa pendente/coleção/fusão ainda não estão completas.

# Primeiro resultado obrigatório: baseline protegido

Antes de criar o projeto Godot:

1. mostra a branch, `HEAD`, remotes e `git status --short`;
2. executa `cabal build all` e `cabal test --test-show-details=direct`;
3. regista o número exato de testes;
4. verifica se `godot`, `godot4` ou um executável equivalente está disponível e regista a versão;
5. cria `docs/04-Migracao-Godot/baseline.md` com data, commit, estado do worktree, comandos, resultados, sistemas existentes e limitações;
6. cria `docs/04-Migracao-Godot/matriz-paridade.md` com estados `nao iniciado`, `parcial`, `paridade automatica`, `paridade manual` e `diferenca aceite`;
7. se o worktree estiver sujo, não tentes criar um commit de checkpoint com alterações alheias: documenta-o e limita as novas alterações a `godot/`, ferramentas de exportação isoladas e documentação de migração;
8. se o utilizador já tiver criado um checkpoint limpo, regista a referência. Não inventes que existe um snapshot.

# Stack alvo

Usa:

- versão estável atual do Godot 4 e regista a versão exata; não mudes de versão a meio da migração;
- GDScript com tipagem estática sempre que os tipos forem conhecidos;
- Windows desktop como primeiro alvo de distribuição;
- `TileMapLayer` e `TileSet` para mapas, separando terreno, decoração e overlays quando útil;
- `Control`, `Container` e Themes para UI responsiva;
- `AnimationPlayer`, Tween, partículas e shaders 2D apenas onde melhorarem feedback e legibilidade;
- `FileAccess`, JSON versionado e `user://` para persistência;
- um runner headless simples em GDScript antes de adicionar frameworks externas de testes;
- cenas pequenas e reutilizáveis, Resources para configuração e lógica de domínio independente da árvore de cenas.

Usa o Compatibility renderer se for suficiente para os requisitos 2D e ampliar compatibilidade. Se escolheres outro renderer, regista a necessidade concreta.

# Estrutura inicial esperada

Podes adaptar nomes, mas mantém separação equivalente:

```text
godot/
  project.godot
  README.md
  export_presets.cfg
  assets/
    generated/
    licensed/
    placeholders/
  data/
    towers/
    enemies/
    waves/
    maps/
  scenes/
    boot/
    menus/
    game/
    entities/
    ui/
    editor/
  src/
    autoload/
    domain/
    gameplay/
    bot/
    persistence/
    presentation/
  tests/
    fixtures/
    unit/
    integration/
    visual/
    run_tests.gd
migration/
  schema/
  fixtures/
  samples/
tools/
  save-exporter/
docs/04-Migracao-Godot/
  baseline.md
  arquitetura.md
  contrato-saves.md
  matriz-paridade.md
  diario-migracao.md
  checklist-cutover.md
```

Não cries dezenas de classes vazias apenas para satisfazer esta árvore. Implementa a estrutura progressivamente, quando houver consumidores reais.

# Arquitetura

Separa três camadas:

1. **Domínio puro**: IDs, specs, estados, comandos, regras, scoring, bot e serialização em Dictionaries tipados/DTOs. Não depende de Nodes.
2. **Aplicação**: coordena partida, conta ativa, save, transições e comandos do jogador/bot.
3. **Apresentação Godot**: cenas, `Node2D`, `Control`, animações, áudio e input. Observa o estado e emite comandos; não contém regras duplicadas.

Não transformes cada dado em estado espalhado por Nodes. Mantém uma fonte de verdade da partida e aplica ações explícitas. O jogador e o bot devem passar pelas mesmas operações de construir, melhorar, especializar, vender e iniciar/preparar vagas.

Mantém a simulação determinística sempre que possível. Usa `_physics_process` ou um relógio fixo para regras de jogo e deixa animação/interpolação na camada visual. Velocidades 1x/2x/4x não podem criar diferenças de resultado por atravessar a base ou saltar efeitos.

# Mapeamento de sistemas

Regista em `arquitetura.md` como cada fonte Haskell passa para Godot:

| Haskell | Godot | Regra de migração |
|---|---|---|
| `LI12425` | DTOs/Resources de domínio | preservar campos, unidades e invariantes |
| `Tarefa1` | validação e rota | fixtures de caminhos válidos/inválidos |
| `Tarefa2` | resolução de efeitos | mesmas combinações e durações |
| `Tarefa3` | simulação fixa | paridade de movimento, disparo, morte e base |
| `EnemySpatial` | índice espacial | não procurar todos os inimigos por torre a cada frame |
| `MapData` | `TileMapLayer` + dados | preservar cinco mapas e IDs |
| `MapEditor` | cena de editor | validar caminho, base, portal e torres |
| `TowerSystem`/`TowerRuntime` | Resources + runtime | identidade explícita, níveis e especialização |
| `EnemySystem` | specs + runtime | armadura, resistências, escudo, cura e bosses |
| `WaveSystem` | Resources/DTOs de vagas | composições e mutadores declarativos |
| `BotStrategy` | domínio puro do bot | primeiro paridade, depois melhorias documentadas |
| `ProgressionSystem` | serviço de progressão | mesmos requisitos e recompensas |
| `ShopSystem` | domínio transacional | nenhuma compra negativa ou recompensa ambígua |
| `AccountStorage`/`SaveSystem` | serviço de persistência | JSON versionado, atómico e recuperável |
| `Desenhar`/`UI*` | cenas, Themes e animação | preservar informação, melhorar composição |

# Contrato de dados e migração de saves

Não tentes interpretar toda a gramática de `Show/Read` no Godot. Implementa um pequeno exportador Haskell que reutiliza os decoders atuais e produz um formato neutro.

O exportador deve:

- ser uma ferramenta/executável isolado, sem substituir o jogo;
- ler os saves globais legacy e as contas versionadas que realmente existirem;
- usar os tipos e fallbacks atuais para descodificar;
- exportar JSON UTF-8 com `schema`, `version` e IDs estáveis em ASCII minúsculo;
- mapear manualmente o domínio para DTOs; não depender de nomes localizados;
- exportar o mapa por ID quando reconhecido e também a grelha quando for customizado;
- exportar `TowerId`, nível e especialização de cada torre construída;
- exportar perfil, ranking, modo, meta progress, coleção, fusões, loja, recompensa pendente e partida pendente;
- nunca modificar ou apagar a origem;
- devolver erros por ficheiro e continuar com outras contas válidas;
- escrever primeiro num temporário, validar o próprio JSON e só depois promover;
- ter testes com save V2, `Jogo` legacy, ficheiro inválido, backup e duas contas independentes.

Formato conceptual mínimo:

```json
{
  "schema": "immutable-towers-transfer",
  "version": 1,
  "source": {
    "game": "haskell-gloss",
    "exporter_version": 1
  },
  "legacy_global": {},
  "account_index": {},
  "accounts": [],
  "warnings": []
}
```

Define um JSON Schema ou documento equivalente em `migration/schema/`. Campos desconhecidos devem poder ser ignorados de forma segura; campos obrigatórios inválidos devem impedir apenas a conta/partida afetada.

O importador Godot deve:

- mostrar uma pré-visualização antes de importar dados encontrados;
- guardar em `user://` com versão explícita;
- usar `.tmp`, validação e `.bak` antes de substituir;
- calcular uma identidade/hash do pacote importado e ser idempotente;
- nunca substituir uma conta Godot mais recente sem confirmação;
- manter cópia do pacote de transferência e caminho dos originais no relatório;
- permitir reverter para o backup;
- apresentar mensagens humanas para ficheiro ausente, corrompido, versão futura e importação parcial;
- tratar dados locais como dados locais, sem prometer segurança criptográfica ou cloud.

# Fixtures de paridade

Cria uma ferramenta Haskell de fixtures que produza entradas e resultados JSON para os algoritmos puros. O Godot deve consumir os mesmos casos.

Cobertura mínima:

- validação de mapa e caminho portal-base;
- transformação célula/posição e movimento em curvas;
- chegada à base em passos grandes;
- targeting por prioridade;
- dano, área, armadura, resistência, escudo e regeneração;
- efeitos simultâneos e expiração sem inimigos presos;
- custos e resultado de upgrades/especializações;
- composição das ondas e mutadores;
- compras de baús, duplicados e fusão;
- progressão de campanha e modos;
- score e resultado de partida;
- decisão determinística do bot;
- round-trip dos saves e importação legacy.

Não testes apenas que “não crasha”. Compara valores esperados, IDs, listas, scores e estados finais com tolerância explícita para floats.

# Fases de execução

## Fase 0 — Proteção e inventário

- completar o baseline obrigatório;
- criar matriz de paridade e mapa de módulos;
- inventariar assets e licenças;
- confirmar ferramentas disponíveis;
- deixar Haskell sem regressões.

Saída: baseline reproduzível e nenhum ficheiro antigo perdido.

## Fase 1 — Scaffold Godot e testes

- criar `godot/project.godot` e uma cena de boot mínima;
- configurar resolução de design 1920x1080 com comportamento responsivo;
- criar input map para rato, pausa, velocidade, HUD, loja, save/load, bot e editor;
- criar Theme base a partir de `VisualTheme`;
- criar runner headless e primeiro teste puro;
- adicionar `run-godot.ps1`/`.bat` sem alterar `run-game`.

Saída: o projeto abre, executa uma cena mínima e os testes headless terminam com código de saída correto. Se Godot não estiver disponível, não declares esta fase validada.

## Fase 2 — Contrato e fixtures

- implementar DTOs e IDs estáveis;
- criar exportador Haskell e schema de transferência;
- gerar fixtures de paridade;
- implementar leitura/escrita JSON atómica no Godot;
- testar dados válidos, corrompidos e de versão futura.

Saída: Haskell e Godot entendem o mesmo contrato sem depender de `Show/Read` no lado Godot.

## Fase 3 — Vertical slice decisiva

Implementar uma pequena fatia com qualidade real:

- PlanicieSerena;
- portal, rota e base;
- Sentinela, Glaciar e Braseiro;
- Basico, Rapido, Tanque e Dispersor;
- três vagas representativas;
- construir, selecionar, melhorar, vender e cancelar;
- targeting, efeitos, morte, recompensa e dano à base;
- HUD, loja lateral e painel contextual responsivos;
- pausa e 1x/2x/4x;
- bot estratégico a recomendar/executar uma decisão;
- guardar, fechar, reabrir e continuar;
- vitória e derrota;
- placeholders visuais coerentes, animações de feedback e áudio opcional se houver assets legais.

Critério de passagem: fixtures essenciais iguais, 60 FPS no alvo de referência, UI utilizável nas quatro resoluções documentadas e save round-trip. Se a fatia não provar vantagem ou paridade, mantém Haskell como principal e documenta o motivo; não forces a migração total.

## Fase 4 — Gameplay completo

- migrar as nove torres, níveis, prioridades e especializações;
- migrar todas as classes, bosses, resistências e efeitos;
- migrar cinco mapas, asfalto, água, rotação e editor;
- migrar história, infinito, desafio, boss e sandbox;
- migrar ondas declarativas, marcos e mutadores;
- manter a simulação separada do renderer.

Saída: todos os itens de gameplay na matriz com paridade automática ou diferença aceite.

## Fase 5 — Bot estratégico

Primeiro reproduz `BotStrategy` atual com fixtures. Depois melhora sem misturar com a migração base:

- passar contexto com registry, modo, mapa e preview;
- usar rota ordenada e tempo de cobertura;
- comparar construir, upgrade, especialização e poupança;
- atualizar identidade runtime explicitamente;
- explicar a decisão e alternativas;
- cachear análise estática do mapa;
- usar exatamente as mesmas ações e custos do jogador;
- nunca usar informação que o jogador não vê.

Regista separadamente o que é paridade e o que é melhoria nova.

## Fase 6 — Contas, progressão e loja

- terminar o fluxo local de criar, selecionar, entrar, trocar e terminar sessão;
- migrar legacy global para uma conta sem perder progresso;
- guardar perfil, ranking, meta, loja, recompensa e partida por ID estável;
- implementar autosave em transações e checkpoints;
- completar baús, revelação, coleção e fusão;
- permitir exportar/eliminar conta com confirmação;
- não adicionar PIN fictício nem autenticação online.

Saída: duas contas permanecem independentes depois de fechar e reabrir o jogo.

## Fase 7 — Apresentação visual e UX

Usa Godot para melhorar o que Gloss tornava manual:

- mapa por TileMapLayer com leitura clara de terreno;
- torres e inimigos com silhuetas distintas;
- estados idle, disparo, impacto, upgrade, dano e morte;
- partículas limitadas e pooling quando necessário;
- UI por Containers, sem coordenadas duplicadas entre desenho e clique;
- tooltips/almanac, preview de vaga e barra de progresso;
- loja meta com cartões, hover, seleção, revelação e fusão;
- contraste, ícone + cor, redução de efeitos e navegação consistente;
- design base 1920x1080 validado em 1280x720, 1600x900, 1920x1080 e 2560x1440.

Mantém a direção de fantasia defensiva com tecnologia rúnica artesanal. Evita UI genérica de motor, excesso de cartões aninhados, gradientes decorativos sem função, texto minúsculo e assets de estilos incompatíveis.

## Fase 8 — Persistência real e migração do utilizador

- integrar o exportador/importador no fluxo de primeira execução;
- testar com cópias de saves reais e fixtures sanitizadas;
- validar importação parcial e rollback;
- confirmar que o Haskell ainda abre os originais;
- documentar onde cada versão guarda dados;
- criar checklist manual de migração para utilizadores.

Saída: progresso existente reaparece na conta Godot correta e o original continua recuperável.

## Fase 9 — Performance, distribuição e paridade final

- perfilar 1x e 4x, vagas densas e bosses;
- evitar `get_nodes_in_group` ou scans completos repetidos no hot path;
- cachear rota/cobertura e usar índice espacial;
- limitar draw calls, partículas, texto flutuante e alocações por frame;
- criar preset Windows e script de bundle Godot separado;
- testar num diretório limpo sem editor;
- executar regressão Haskell, testes Godot, fixtures, smoke test e checklist visual;
- atualizar README e vault.

Saída: os dois bundles funcionam e a matriz de paridade não contém itens essenciais sem estado verificável.

## Fase 10 — Gate de cutover

Não mudes o launcher principal automaticamente. Produz `checklist-cutover.md` com:

- funcionalidades completas e incompletas;
- diferenças de gameplay;
- resultado dos testes e playtests;
- compatibilidade de saves;
- desempenho e requisitos;
- plano de rollback;
- recomendação `aprovar`, `adiar` ou `rejeitar`.

Só depois de aprovação explícita podes propor que Godot passe a versão principal. Mesmo após aprovação, conserva um launcher Haskell e a referência/tag do baseline.

# Regras técnicas específicas

- Usa `Vector2i` para células e converte para posição visual num único serviço; evita igualdade frágil de floats.
- Preserva o significado atual de centros de célula `(x + 0.5, y + 0.5)` nas fixtures de conversão.
- Não dependas da ordem dos Nodes para identidade ou targeting.
- Usa IDs string estáveis como `sentinela`, `boss_ruptura` e `planicie_serena` nos saves.
- Mantém configuração de torres/inimigos/ondas orientada a dados e com uma única fonte de verdade.
- Não dupliques custo, nome, raridade ou cor entre loja, gameplay e UI.
- Usa sinais para eventos de apresentação, não para esconder regras centrais distribuídas.
- Guarda apenas estado necessário; não serializes Nodes inteiros.
- Valida números: sem NaN, infinito, ciclos não positivos, velocidades negativas ou créditos abaixo de zero.
- Faz saves em checkpoints seguros e depois de transações importantes, sem depender apenas do fecho da janela.
- Mantém randomização reproduzível quando afetar recompensas, ondas ou testes.

# Validação contínua

Depois de cada fase:

1. executa `cabal build all`;
2. executa toda a suite Haskell;
3. executa testes Godot headless, quando o motor estiver disponível;
4. executa fixtures de paridade afetadas;
5. abre as cenas alteradas e verifica erros do editor/runtime;
6. captura ou inspeciona visualmente as resoluções relevantes quando houver UI;
7. atualiza `matriz-paridade.md` e `diario-migracao.md`;
8. corrige regressões antes de avançar.

Exemplo de comando headless, adaptando o nome do executável encontrado:

```powershell
godot --headless --path godot --script res://tests/run_tests.gd
```

Não declares “Godot OK” se apenas os ficheiros foram gerados sem abrir/importar o projeto.

# Comportamento perante bloqueios

Continua autonomamente perante decisões reversíveis de estrutura ou nomes. Para em apenas estes casos:

- instalação de Godot/SDK/addon que exija alteração externa ao repositório;
- operação Git que incluiria alterações preexistentes do utilizador;
- eliminação, substituição ou movimento de dados/código antigo;
- asset sem direitos claros que seja indispensável para distribuição;
- incompatibilidade de save que possa perder progresso;
- escolha de produto que altere gameplay ou âmbito de forma material.

Quando parares, apresenta o bloqueio exato, tudo o que já validaste e a menor decisão necessária. Não uses um bloqueio visual para abandonar trabalho de domínio, fixtures ou documentação que ainda possa avançar em segurança.

# Entrega final

Entrega:

- resultado da migração e recomendação de cutover;
- fases concluídas e respetivos critérios;
- lista de ficheiros adicionados/alterados;
- estado do Haskell e número exato de testes;
- versão do Godot e resultados headless;
- matriz de paridade resumida;
- resultado do export/import de saves e rollback;
- instruções para executar Haskell e Godot;
- instruções para criar bundles Windows;
- validações manuais ainda abertas;
- riscos e próximos três passos de maior impacto.

Começa agora pelo baseline protegido e inventário. Não comeces por apagar, converter assets em massa ou reescrever todos os sistemas de uma vez.

## Referências técnicas verificadas

- [Godot 4.7 — TileMapLayer](https://docs.godotengine.org/en/4.7/tutorials/2d/using_tilemaps.html)
- [Godot — Containers e UI responsiva](https://docs.godotengine.org/en/latest/tutorials/ui/gui_containers.html)
- [Godot — AnimationPlayer](https://docs.godotengine.org/en/stable/tutorials/animation/introduction.html)
- [Godot — ficheiros e `user://`](https://docs.godotengine.org/en/stable/tutorials/io/data_paths.html)
- [Godot — execução headless e scripts](https://docs.godotengine.org/en/stable/tutorials/editor/command_line_tutorial.html)
- [Godot — licença MIT](https://godotengine.org/license/)
- [OpenAI — GPT-5.6 Sol](https://developers.openai.com/api/docs/models/gpt-5.6-sol)
