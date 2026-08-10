# Immutable Towers - Diário da Migração

Tags: #migracao #diario #evidencia

Relacionadas: [[baseline]], [[matriz-paridade]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Última Auditoria]]

## 2026-07-13

- baseline Git/Haskell registado;
- 166/166 testes Haskell confirmados;
- Godot ausente do PATH; copia portatil oficial autorizada e instalada fora do repositorio;
- alvo confirmado como Godot 4.7 stable;
- scaffold lado a lado criado em `godot/`;
- launcher Haskell mantido intacto;
- importacao headless e smoke test passaram;
- runner headless passou 79 checks de IDs, fixtures, importacao, persistencia, mapas, modos e simulacao;
- schema JSON v1 e fixtures neutros criados em `migration/`;
- launcher e testes Godot detetam a copia portatil e propagam o exit code real;
- exportador Haskell isolado compila, escreve por `.tmp` e valida o proprio JSON;
- save meta real foi exportado apenas para `%TEMP%`, sem alterar a origem;
- planeador de importacao protege repeticoes e progresso Godot mais recente;
- catalogo canónico passou a ser gerado pelo Haskell com nove torres, onze inimigos, cinco mapas e cinco modos;
- as cinco rotas sao calculadas a partir das grelhas exportadas e validadas sem atravessar agua;
- a partida permite construir as nove torres, selecionar, melhorar, especializar, vender, guardar, carregar e usar 1x/2x/4x;
- Historia usa as dez vagas reais; Desafio, Boss e Sandbox usam as composicoes reais;
- o Infinito gera vagas sem limite e as primeiras dezoito foram comparadas atributo a atributo com o Haskell, incluindo precisao `Float` de 32 bits;
- armadura, resistencias, escudo, regeneracao, efeitos, sinergias e comportamentos dos tres bosses estao ativos;
- grelha espacial reutilizavel remove o scan global torre por todos os inimigos no targeting;
- bot inicial deterministico sugere e executa comandos pela mesma API usada pelo jogador;
- save de partida versionado usa escrita temporaria, backup e recuperacao de corrupcao;
- cena validada por importacao, instanciacao, simulacao deterministica e captura visual a 1280x720;
- gravacao `--write-movie` em renderer dummy causou crash interno no Godot 4.7; a validacao visual foi feita numa janela real;
- assets antigos excluidos do scaffold por proveniencia incerta.

## 2026-07-15

- suite Haskell reconfirmada: build OK e 166/166 testes;
- suite Godot expandida para 150 checks, sem erros;
- menu completo, contas locais, ranking, modos bloqueados, loja, recompensa pendente e fusao ligados ao `AppState`;
- partidas pendentes passaram a pertencer a conta ativa, com Continuar/Nova Partida e checkpoints automaticos;
- exportacao/eliminacao de contas e importacao idempotente com confirmacao testadas;
- runs Haskell passam por `LegacyRunConverter`, incluindo runtime de torre, inimigos ativos, efeitos e vagas restantes;
- editor guarda, carrega e inicia mapas personalizados; snapshots conservam a grelha custom;
- bot compara todas as torres desbloqueadas, upgrades, especializacoes, counters e cobertura; cinco referencias Haskell coincidem;
- feedback visual inclui hit flash, numeros de dano, construcao, upgrade, venda e morte;
- HUD ganhou preview e barra de progresso de vaga; painel ganhou tooltips e preview numerico de upgrade;
- economia, escalamento e geracao infinita foram separados em modulos puros;
- impacto, projeteis/efeitos e runtime de inimigos foram separados em modulos puros;
- fixtures geradas pelo Haskell cobrem 98 tiers, 45 precos, 99 pares de impacto, 8 sequencias de sinergia, 7 passos temporais e 5 casos de chegada a base;
- cena real validada a 1280x720 sem sobreposicao nem controlos de desenvolvimento;
- cena real validada tambem a 1600x900, 1920x1080 e 2560x1440 sem cortes ou sobreposicoes;
- benchmark estabilizado de 600 inimigos/120 torres: media 3.46 ms, p95 4.28 ms; pico inicial de warmup fica registado para investigacao;
- bundle Godot portatil inicial executou fora do projeto sem ambiente Haskell/Godot instalado.
- opcoes reais por conta passaram a controlar ecra inteiro, reducao de efeitos e numeros de dano;
- ajuda ganhou almanaques rolaveis de torres e inimigos, derivados do catalogo gerado;
- ajuda ganhou creditos e a lista completa dos atalhos de partida;
- obstaculos dinamicos passaram a abrandar inimigos sem interromper a rota e sobrevivem ao save/load;
- HUD e painel lateral passaram a poder ser recolhidos por botao ou pelos atalhos H/K;
- barra superior foi revalidada a 1280x720 depois destes controlos, sem cortes ou sobreposicoes;
- bundle Windows final foi reconstruido, extraido num diretorio temporario e arrancado sem depender de Haskell, Cabal ou da instalacao de Godot;
- ZIP final: 84 033 418 bytes, SHA-256 `77116B49E8078EC24120820E6693245C29837284C76C8FFDBF7FA162EA4A274D`;
- copia do save Haskell real foi importada pelo fluxo da UI num armazenamento isolado; a origem manteve o SHA-256 `2958B01211B84E29D9AE9E4594C7982906225AA94F75AA563AD8B0AACB5601FA`.

## 2026-07-28

- auditoria completa ao código, vault, Git, testes e bundle;
- Haskell reconfirmado: build OK e 166/166 testes;
- Godot reconfirmado: 150/150 checks em Godot 4.7, sem erros de engine;
- ZIP final reconfirmado com 84 033 418 bytes e SHA-256 `77116B49E8078EC24120820E6693245C29837284C76C8FFDBF7FA162EA4A274D`;
- três benchmarks de 600 inimigos/120 torres ficaram entre 3,21 e 3,26 ms de média, 2,96 e 3,20 ms de p95, com pico frio entre 74,7 e 83,0 ms;
- vault validado com zero wikilinks partidos e zero nomes duplicados;
- confirmado que o worktree continua sem checkpoint desde `069e82c`;
- detetado bloqueador: `export_account` gera `immutable-towers-account-export`, mas o importador aceita apenas `immutable-towers-transfer`;
- detetado bloqueador: ações automáticas do bot não chamam checkpoint imediato;
- confirmado que uma conta nova oferece apenas Sentinela, explicando parte da repetição observada no bot;
- confirmado que as cinco referências cruzadas do bot são executadas com apenas Sentinela e não provam diversidade;
- checklist de cutover voltou a explicitar gates de contas, persistência, linguagem de produto, playtest e segundo computador;
- criado [[02-Planeamento/plano-proximas-fases|Plano das Próximas Fases]] e registada a evidência em [[03-Qualidade-e-Release/auditoria-2026-07-28|Auditoria 2026-07-28]];
- decisão mantida: cutover `ADIAR`.
- envelope nativo Godot passou a fazer round-trip com colisões explícitas: cancelar, importar como nova ou substituir;
- IDs de conta passaram a usar `Crypto`, primeira execução ganhou onboarding e a sessão pode ser terminada/lembrada;
- autosave passou a cobrir ações do jogador e bot, vaga, pausa, menu, foco e fecho;
- Bot 2.0 ganhou leitura de defesas, controlo, cobertura marginal, redundância, ganho marginal, poupança e explicação do score;
- Sandbox passou a expor o catálogo completo ao bot;
- tema ganhou estados normal, hover, pressed, focus e disabled; loja ganhou cards e requisitos visíveis;
- UI validada também em 1366x768, 1280x800 e 2560x1080;
- suite Godot expandida para 176 checks;
- vault limpo dos cinco rascunhos vazios e notas vivas sincronizadas com a implementação.
- bundle reconstruído, extraído e iniciado em modo isolado com exit code 0;
- ZIP atualizado: 84 053 630 bytes, SHA-256 `A32D859AEF8B3C54A7316E91A85DBD99A0CA144E737BA0369206C9AE5442E914`.
- capacidade de construção adicionada por modo: História 18, Infinito 24, Desafio 12, Bosses 16 e Sandbox 30;
- cada torre passou a ocupar nove células de relva numa base 3x3; preview, seleção, fundação e Bot 2.0 usam o mesmo footprint;
- restore mantém saves antigos mesmo com torres incompatíveis com a geometria nova ou acima do limite; venda liberta capacidade;
- célula visual centralizada em `GameLayout` e reduzida de 24 para 22 px, preservando grelha, rotas e coordenadas;
- loja recebeu componentes próprios com modelos de Madeira/Cristal/Imperial, abertura, revelação de nova/duplicada, compensação e `reduced_effects`;
- menu recebeu cenário procedural em camadas com estrada, portal, inimigos, torres, projéteis e partículas;
- suite Godot expandida para 200 checks; Haskell reconfirmado em 166/166;
- menu, loja e mapa/editor revalidados numa janela real a 1280x720;
- bundle final extraído para `%TEMP%` e iniciado diretamente sem dependências de desenvolvimento;
- ZIP final desta ronda: 84 077 018 bytes, SHA-256 `8C65885C225A5DDFB34CD6D9954D402B6862A2EA6AEF16A38C5BBB4F4A4FEFE7`.
- clarificação de escala: a iteração “torre com footprint 3x3 sobre grelha 36x34” foi substituída; não representa a intenção final;
- criado `WorldGrid`: cada bloco 3x3 do catálogo-fonte passa a uma célula global, produzindo uma grelha Godot 12x11 com células de 66 px;
- torres voltaram a ocupar uma célula; estrada, asfalto, água, inimigos, portal, base e obstáculos usam a mesma escala global;
- movimento, alcance, auras e raio terminal convertem distâncias por `1/3`, preservando aproximadamente a escala física e os tempos;
- snapshots marcam `world_grid_scale = 3`; runs e mapas personalizados antigos são migrados sem eliminar torres;
- Bot 2.0 passou a calcular cobertura e redundância em células globais;
- suite Godot expandida para 211 checks; Haskell permanece em 166/166;
- nova grelha capturada numa janela real a 1280x720 e 1920x1080.
- benchmark sintético manteve 600 inimigos/120 torres: três execuções ficaram em 5,19-5,40 ms de média, p95 6,72-7,30 ms e máximo 38,7-42,8 ms.
- bundle da correção extraído e iniciado isoladamente com exit code 0 e sem erros de motor;
- ZIP atualizado: 84 083 315 bytes, SHA-256 `EA5965D5F02B43DA8AB84E7157BA5552E2CD1351BCCA3A4018A6640288BE215B`.
## 2026-07-29 - Integridade de gameplay e gate

- economia passa a guardar compra, upgrades e investimento efetivamente pagos;
- refund fixado em 65%, com migração conservadora de saves antigos;
- controlo extraído para política com caps, tenacidade, diminishing returns e velocidade mínima;
- score extraído para serviço puro e deixa de premiar espera ou quantidade de torres;
- primeira proteção do Bot 3.0: poupança válida, atingível e dependente de defesa mínima;
- suite Godot expandida para 234 checks;
- `run-tests.ps1` executa também um gate de 45 transações e nove partidas finitas;
- Bot 3.0 ganhou planeador com memória, rollout reduzido e reserva para três defesas iniciais;
- `RunDifficultyProfile` diferencia os cinco estágios e preserva o estágio 1 como referência;
- os cinco estágios do capítulo 1 vencem automaticamente sem stalemate;
- benchmark de 600 inimigos/120 torres mantém p95 abaixo de 8 ms;
- ZIP atualizado: 84 103 347 bytes, SHA-256 `E711BB64820666F8AA761552C704C07ABDCEC6638D45848ADD0BA6F32806FA4E`;
- bundle extraído e iniciado com sucesso numa pasta temporária isolada;
- launcher da raiz continua em Haskell e o bundle permanece de desenvolvimento até existirem templates de export release.

## 2026-07-30 - Economia do Infinito

- criada uma fonte única de recompensas em `src/gameplay/run_rewards.gd`;
- o Infinito atribui gemas pela vaga alcançada mesmo quando a base é derrotada;
- curva recalibrada para 2,5x com retornos decrescentes: 30 gemas na vaga 10, 70 na 20, 125 na 50 e limite de 188 na vaga 120;
- melhor vaga do Infinito persistida por conta e apresentada corretamente no ranking;
- resultado final mostra a recompensa de gemas;
- contas antigas recebem `best_infinite_wave` ao concluir a primeira run de Infinito, sem reset;
- suite Godot expandida para 250 checks; gate de gameplay permanece aprovado.
- ZIP atualizado após a recalibração: 84 105 093 bytes, SHA-256 `4466E9604CFDEE31EA58713E63429570E84D54682C7F2694309B6D1AFE8C98DC`;
- bundle extraído e iniciado isoladamente com exit code 0.

## 2026-08-10 - Checkpoint consolidado

- revisto o diff completo da migração lado a lado, sistemas Haskell, Godot, fixtures, exportador e testes;
- vault confirmado com 39 notas, zero links partidos, zero nomes duplicados e tags em todas as notas;
- removido do pacote Git o backup gerado `domain-catalog-v1.json.bak`;
- roadmap, backlog, estado atual e checklist de cutover sincronizados com a evidência existente;
- checkpoint preparado para publicação na `main`, mantendo builds, caches, saves e bundle fora do repositório.
