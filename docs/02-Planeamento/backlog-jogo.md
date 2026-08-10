# Immutable Towers - Backlog

Tags: #backlog #prioridade

Esta nota contém apenas trabalho em aberto. Estado apresentável: [[roadmap-atual]]. Ordem e critérios: [[plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]. Evidência: [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]].

## P0 - antes de novas features

- [ ] criar um export Windows real com template e `--export-release`, sem `(DEBUG)`;
- [ ] validar duas contas, partida pendente e restauro do backup num segundo computador físico;
- [ ] completar resize e escala Windows 100%/125%/150%;
- [ ] aprovar explicitamente o cutover antes de alterar o launcher principal.

## P1 - Bot 3.0

- [ ] provar e afinar a comparação entre upgrade e construção antes de preencher a capacidade;
- [ ] terminar a extração de contexto e geração de candidatos do planeador;
- [ ] calibrar rollout, memória e posicionamento até superar a baseline;
- [ ] reagir a eventos de vaga/arsenal/torre em vez de recalcular apenas por intervalo;
- [ ] registar previsão, decisão e resultado local sem dados pessoais;
- [ ] provar que arsenal completo iguala ou supera Sentinela-only nos cinco mapas;
- [ ] cobrir todas as classes, mutadores e bosses.

## P1 - campanha, modos e balanceamento

- [ ] dar identidade mecânica e baseline própria a cada mapa;
- [ ] tornar Desafio vencível e distinto por restrição clara;
- [ ] voltar a equilibrar Bosses apenas depois da correção de controlo;
- [ ] medir e ajustar a eficiência universal da Sentinela;
- [ ] garantir um cenário forte e um cenário fraco para cada torre;
- [ ] validar duração, vitória, leaks, economia e diversidade em campanhas completas.

## P1 - UI responsiva e acessibilidade

- [ ] substituir offsets fixos da partida por contentores, anchors e layout centrado;
- [ ] tornar 1024x768 legível e reorganizar 4:3/21:9;
- [ ] adicionar escala de UI 80-150%;
- [ ] criar cards de torre com ícone, custo, papel e estado;
- [ ] transformar o preview de vaga em ícones, quantidades e perigo;
- [ ] completar navegação por teclado, foco e remapeamento;
- [ ] usar forma/ícone/texto além de cor para estados;
- [ ] rever contraste, tamanhos de alvo, reduced effects e intensidade de VFX.

## P1 - loja, modelos e áudio

- [ ] aumentar protagonismo dos modelos de baú e remover mensagens sobrepostas;
- [ ] testar interrupção/restart em todos os momentos da abertura;
- [ ] criar grelha de coleção e detalhe por torre;
- [ ] criar peças estruturais por nível e duas silhuetas de especialização;
- [ ] reforçar modelos/telegraphs de classes e bosses;
- [ ] iluminar e enquadrar melhor o fundo animado do menu;
- [ ] criar orçamento de VFX para alta densidade;
- [ ] integrar áudio apenas com origem/licença registada e sliders funcionais.

## P1 - conta e experiência de sessão

- [ ] testar criar, renomear, trocar, eliminar, exportar e importar por UI;
- [ ] validar duas contas com coleções, rankings, opções e partidas diferentes após restart;
- [ ] validar backup num segundo computador;
- [ ] manter login/cloud como projeto separado até existir decisão de backend.

## P2 - conteúdo diferenciador

- [ ] prototipar `Contratos de Defesa` apenas depois dos gates de gameplay;
- [ ] medir um loadout de cinco torres apenas depois de equilibrar as nove;
- [ ] não iniciar multiplayer, heróis, raridades aleatórias ou cloud obrigatória nesta fase.

## P2 - arquitetura, CI e apresentação futura

- [ ] extrair responsabilidades de `vertical_slice_simulation.gd`;
- [ ] separar input/HUD/persistência de `vertical_slice_screen.gd`;
- [ ] separar loja/coleção/perfil/ranking de `boot_screen.gd` quando a feature tocar na área;
- [ ] dividir `run_tests.gd` por domínio;
- [ ] criar CI para Haskell, Godot, import, harness, benchmark e export release;
- [ ] criar asset manager apenas com proveniência/licença confirmada;
- [ ] integrar fonte licenciada com fallback;
- [ ] avaliar IDs únicos de inimigo apenas se a telemetria precisar de rastreio individual;
- [ ] limpar encoding de notas históricas sem reescrever o seu sentido.

## Validações manuais de release

- [ ] comprar/vender em todos os modos sem aumentar saldo;
- [ ] terminar campanhas, Desafio e Bosses sem stalemate;
- [ ] comparar bot completo com Sentinela-only;
- [ ] 1280x720, 1366x768, 1280x800, 1600x900, 1920x1080, 2560x1440 e 21:9;
- [ ] 1024x768 e um rácio 4:3;
- [ ] redimensionamento da janela durante menu e partida;
- [ ] escala Windows 100%, 125% e 150% quando disponível;
- [ ] navegação completa por rato e teclado;
- [ ] export release sem título `(DEBUG)` num segundo computador sem Haskell, Cabal ou Godot;
- [ ] exportar a conta no primeiro computador e restaurar no segundo;
- [ ] atualizar e aprovar [[04-Migracao-Godot/checklist-cutover|Checklist de Cutover]].
