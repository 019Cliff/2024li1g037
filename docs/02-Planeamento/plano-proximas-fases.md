# Immutable Towers - Plano das Próximas Fases

Tags: #roadmap #plano #prioridade #godot

Relacionadas: [[00-Inicio/estado-atual|Estado Atual]], [[backlog-jogo|Backlog]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]], [[04-Migracao-Godot/checklist-cutover|Checklist de Cutover]]

> [!warning] Plano anterior
> Esta nota preserva o planeamento fechado em 2026-07-28. A auditoria prolongada encontrou arbitragem económica, stalemates e regressões estratégicas do bot. O plano vigente é [[plano-melhoria-total-2026-07-29|Plano de Melhoria Total - 2026-07-29]].

## Decisão estratégica

Continuar a evolução em Godot 4.7, lado a lado com a versão Haskell. A migração funcional está suficientemente avançada para que voltar a investir no renderer Gloss tenha baixo retorno. O Haskell mantém três papéis:

1. referência de regras e fixtures;
2. exportador de dados/saves;
3. rollback até o cutover ser aprovado.

O próximo ciclo não deve começar por adicionar mais sistemas. Primeiro protege-se o trabalho existente e garante-se que contas e saves não perdem dados. Depois melhora-se o bot. Só então se faz a grande ronda visual, da loja e dos modelos.

## Ordem obrigatória

`checkpoint Git -> integridade de contas/saves -> bot 2.0 -> UI/loja/modelos -> balanceamento/playtest -> cutover`

Uma fase só avança quando os critérios de saída da anterior estiverem cumpridos.

## Estado de execução em 2026-07-28

| Fase | Estado |
|---|---|
| 0 - Proteger | Parcial: rascunhos limpos e diff compreendido; checkpoint Git aguarda pedido explícito |
| 1 - Contas/saves | Implementação automática concluída; falta playtest em dois computadores |
| 2 - Candidato | Textos e confirmação concluídos; extração arquitetural continua gradual |
| 3 - Bot 2.0 | Primeira versão estratégica concluída; faltam memória, telemetria e cenários completos |
| 4 - Visual | Loja com baús/revelação, fundo do menu e mapa compacto concluídos; coleção, cards de torres e telegraphs continuam abertos |
| 5/6 - QA/cutover | Em aberto |

## Fase 0 - Proteger e consolidar o estado atual

Prioridade: **P0**

### Trabalho

- rever todo o diff sem apagar alterações preexistentes;
- separar ficheiros fonte de gerados e temporários;
- confirmar que `.godot/`, `release/` e outputs de build continuam ignorados;
- rever e remover apenas os rascunhos vazios confirmados;
- criar uma branch/checkpoint da migração lado a lado;
- organizar commits por âmbito:
  1. melhorias Haskell e testes;
  2. projeto Godot, fixtures e exportador;
  3. reorganização e atualização do vault;
- registar o commit exato no estado atual e no diário de migração.

### Critérios de saída

- `git status` compreendido e sem ficheiros importantes apenas no disco local;
- Haskell continua com 166/166 testes;
- Godot continua com 211/211 checks;
- o bundle e os saves locais não são introduzidos acidentalmente no Git;
- existe um ponto de rollback reproduzível.

## Fase 1 - Integridade de contas, backup e autosave

Prioridade: **P0**

### 1.1 Round-trip de conta Godot

- aceitar o schema `immutable-towers-account-export` no importador ou substituir os dois formatos por um envelope comum e versionado;
- validar schema e versão antes de escrever;
- preservar perfil, progressão, gemas, coleção, fusões, ranking, opções, recompensa e partida pendentes;
- decidir colisões por `AccountId` com três resultados explícitos: importar como nova, substituir após confirmação ou cancelar;
- nunca substituir silenciosamente uma conta com progresso;
- manter `.tmp` e `.bak`;
- permitir restaurar num armazenamento vazio.

### 1.2 Fluxo de conta local

- mostrar onboarding/criação quando não existem contas;
- acrescentar `Trocar conta`/`Terminar sessão` sem sugerir segurança inexistente;
- usar identificadores realmente únicos em vez de depender apenas do segundo atual;
- expor “lembrar última conta” se o campo continuar no índice;
- distinguir na UI:
  - conta local neste computador;
  - exportar/importar para backup;
  - sem sincronização cloud.

### 1.3 Autosave completo

Criar checkpoint após:

- construção, upgrade, especialização, venda e obstáculo do jogador;
- qualquer ação mutável bem-sucedida do bot;
- início/fim de vaga;
- pausa e regresso ao menu;
- perda de foco/fecho normal da aplicação;
- temporizador moderado durante uma vaga longa.

O autosave deve ser agregado para evitar escrita a cada frame. Um pequeno `SaveCoordinator` pode marcar o estado como sujo e gravar em pontos seguros.

### Testes obrigatórios

1. criar contas A e B, progredir de forma diferente, fechar/reabrir e confirmar independência;
2. exportar A, importar num repositório vazio e comparar o documento normalizado completo;
3. testar colisão de ID sem overwrite silencioso;
4. corromper o ficheiro principal e recuperar do `.bak`;
5. comprar baú, sair durante a recompensa e recuperar sem duplicar nem perder gemas;
6. deixar o bot construir, fechar antes da vaga seguinte e confirmar a torre restaurada;
7. importar uma cópia Haskell e confirmar que a origem não muda.

### Critérios de saída

- zero perda de progresso nos cenários acima;
- exportar e importar uma conta Godot funciona na própria UI;
- toda ação económica tem persistência verificável;
- textos deixam claro que as contas são locais.

## Fase 2 - Limpeza de candidato a release

Prioridade: **P0/P1**

### Trabalho

- remover do jogador os termos `scaffold`, `vertical slice`, `fatia vertical` e “migração”;
- renomear textos de estado e README do bundle para linguagem de produto;
- confirmar pausa, reinício, vitória, derrota, continuar e nova partida;
- adicionar confirmação quando `Nova Partida` descarta uma partida pendente;
- confirmar que completar/reiniciar não duplica recompensas;
- atualizar o README raiz para apresentar Haskell e Godot sem confusão;
- dividir o runner Godot por domínios sem alterar o resultado agregado;
- começar a extrair `boot_screen`, `vertical_slice_screen` e `vertical_slice_simulation` apenas onde a próxima feature precise.

### Critérios de saída

- nenhum texto de protótipo aparece no bundle;
- todos os caminhos de saída preservam ou descartam progresso de forma explícita;
- testes continuam verdes;
- a arquitetura fica mais fácil de evoluir sem uma reescrita total.

## Fase 3 - Bot estratégico 2.0

Prioridade: **P1**

Objetivo: escolher a melhor ação contextual entre construir, melhorar, especializar ou poupar, com explicação e comportamento determinístico.

### Diagnóstico a mostrar primeiro

Antes de alterar a estratégia, a UI deve mostrar o arsenal disponível. Uma conta nova tem apenas `Sentinela`; nesse caso, repetir a mesma torre é uma restrição de progressão, não necessariamente uma decisão errada do bot. Decidir também se o modo Sandbox deve disponibilizar todas as torres para teste.

### Contexto de decisão

Criar um `BotContext` imutável com:

- mapa e rota ordenada;
- modo, vaga atual e duas vagas seguintes;
- inimigos ativos, posição, vida, velocidade, armadura, resistências, escudo, regeneração, boss e ameaça à base;
- torres, nível, especialização, cooldown, cobertura e papel;
- créditos atuais, rendimento previsto e preços futuros;
- vida da base;
- últimas decisões e objetivo de poupança;
- arsenal realmente desbloqueado.

### Candidatos

- construir cada torre desbloqueada em posições legais relevantes;
- melhorar cada torre;
- escolher especialização quando necessário;
- poupar para um alvo concreto;
- deixar venda/reposicionamento fora da primeira versão, salvo cenário seguro e testado.

Não limitar posições apenas a células adjacentes à rota. Gerar posições dentro do alcance útil, pré-calcular características por mapa e manter um limite de candidatos para performance.

### Score recomendado

Cada candidato deve devolver score total e decomposição:

`utilidade = ameaça neutralizada + cobertura marginal + sinergia + emergência - redundância - desperdício - custo de oportunidade`

Componentes:

- DPS efetivo por inimigo, incluindo rajada/ciclo e defesa real;
- valor de slow, medo, stun, dano prolongado e chain/área;
- tempo de contacto esperado com a rota;
- cobertura de curvas, segmentos longos e zonas repetidas;
- cobertura **nova**, descontando zonas já bem defendidas;
- risco de fuga até à base;
- ganho marginal do upgrade, nunca apenas dano total/custo;
- diversidade apenas quando melhora counters, sem quotas artificiais;
- reserva económica para uma ação futura claramente superior;
- histerese para o bot não trocar de objetivo a cada segundo.

### Cadência

- decidir por evento: início de vaga, mudança forte de ameaça, créditos suficientes para o objetivo ou torre eliminada/vendida;
- tornar o cooldown independente de 1x/2x/4x;
- não executar quatro vezes mais ações por segundo real a 4x;
- guardar checkpoint após ação.

### Explicabilidade e telemetria

Mostrar:

- ação escolhida;
- razão curta;
- custo;
- três componentes principais do score;
- duas melhores alternativas;
- motivo para poupar.

Registar telemetria local de desenvolvimento:

- construção vs upgrade vs poupança;
- torre e posição;
- score por componente;
- ameaça observada;
- resultado da vaga;
- créditos desperdiçados e dano sofrido.

Não incluir dados pessoais.

### Cenários automáticos mínimos

- enxame favorece área/chain;
- rápido favorece controlo com cobertura antes da base;
- blindado favorece o counter correto;
- regenerador favorece burst/foco;
- dispersor penaliza área;
- protegido considera escudo;
- cada boss provoca resposta diferente;
- upgrade vence construção quando o ganho marginal é superior;
- construção vence upgrade quando falta cobertura;
- poupar vence compra fraca quando existe objetivo próximo;
- mapa com curva/múltiplos segmentos escolhe posição de maior contacto;
- torre redundante perde para um papel em falta;
- 1x e 4x produzem a mesma sequência estratégica para o mesmo estado;
- conta apenas com Sentinela continua legal e explica a limitação;
- ação do bot sobrevive a restart.

### Critérios de saída

- o bot passa todos os cenários determinísticos;
- não repete uma torre sem que a decomposição do score justifique;
- alterna compra, upgrade e poupança em playtest prolongado;
- não gasta até zero por reflexo;
- não cria regressão relevante no benchmark.

## Fase 4 - Loja, UI, modelos e direção visual

Prioridade: **P1**, depois da integridade de dados

Objetivo: substituir a apresentação de protótipo por uma identidade coerente sem mudar silenciosamente regras ou economia.

Estado em 2026-07-28: a primeira ronda da loja e o fundo animado do menu estão implementados. O mapa foi convertido para uma grelha global 12x11: cada célula equivale a 3x3 células-fonte e torres, terreno, estrada, inimigos, portal, base e obstáculos partilham a mesma escala. Não foi adicionado outro sistema inspirado noutros jogos: a nova densidade espacial e o limite por modo já criam a escolha estratégica necessária sem aumentar o âmbito.

### Design system

- expandir o `Theme` com tokens semânticos de fundo, superfície, borda, texto, destaque, perigo, sucesso, raridade e estados;
- definir estilos de botão normal, hover, pressed, focus, disabled e compra impossível;
- escolher uma família tipográfica licenciada e incluir fallback;
- definir escala de espaçamento, raios, contornos, sombras e duração de animações;
- garantir contraste e alternativa não cromática;
- manter opção de efeitos reduzidos.

### Loja meta

Estado da substituição do `RichTextLabel` e dos cinco botões genéricos:

- [x] cabeçalho com gemas e coleção;
- [x] três cartões de baú com silhueta, pool, custo e estado;
- [x] hover/press, flutuação, brilho e materiais diferentes;
- [x] sequência transacional pagar -> abrir -> revelar -> novo/duplicado -> recolher;
- [x] recompensa pendente reabre exatamente no estado correto;
- [ ] grelha de coleção com bloqueado/desbloqueado, filtros e detalhe;
- [x] fusão Tempestade com requisitos visuais e materiais presentes/em falta;
- [x] animações curtas e compatíveis com `reduced_effects`;
- [x] lógica económica permanece em `MetaShop`, fora do renderer;
- [ ] decidir por playtest se é necessária confirmação antes de pagar.

### Modelos de torres

Manter silhuetas distintas e criar evolução por três camadas:

1. base estrutural reconhecível;
2. arma/energia ligada ao papel;
3. detalhes de nível/especialização.

Cada especialização precisa de uma diferença visível à distância. Evitar apenas aumentar escala ou trocar cor. Criar componentes reutilizáveis, animação idle discreta, antecipação de disparo, recoil/recuperação e impacto coerente.

### Inimigos e bosses

- silhueta e movimento próprios por classe;
- estado resistente, escudo, regeneração e controlo legíveis;
- telegraph do boss antes da mudança de fase;
- Nexo da Ruptura com segunda fase visual clara sem alterar balanceamento na primeira ronda;
- limitar efeitos simultâneos para preservar leitura em vagas densas.

### HUD e partida

- cards de torre com ícone, custo, papel e tecla;
- seleção com alcance, alvo, DPS efetivo e preview de upgrade;
- comparação atual -> novo para todos os stats relevantes;
- preview de vaga com ícones/counters, não apenas texto;
- aviso de base e inimigo perigoso;
- feedback de autosave discreto;
- layout por Containers/componentes, evitando coordenadas espalhadas.

### Áudio

Não expor opções de áudio até existir sistema real. Quando entrar:

- buses Master, Music, SFX e UI;
- sons licenciados ou produzidos para construir, disparar, impacto, morte, compra, erro e recompensa;
- limites de vozes e variação de pitch moderada;
- ducking leve na revelação/boss;
- sliders persistentes por conta.

### Critérios de saída

- loja deixa de parecer uma página de texto;
- torres são reconhecíveis por silhueta, nível e especialização;
- estados de combate são legíveis sem depender só de cor;
- animações não bloqueiam input nem transações;
- 60 FPS no cenário normal e benchmark sem regressão material;
- validação visual nas resoluções e rácios definidos na Fase 5.

## Fase 5 - Balanceamento, QA e playtest

Prioridade: **P1/P2**

### Matriz de ecrã e input

- 1280x720;
- 1366x768;
- 1280x800;
- 1600x900;
- 1920x1080;
- 2560x1440;
- um 21:9;
- janela redimensionada durante menu e partida;
- escala Windows 100%, 125% e 150% quando disponível;
- rato e teclado completo; gamepad apenas se for assumido como requisito.

### Sessões de jogo

- cinco mapas em História;
- Infinito a 1x e 4x até pelo menos dois marcos combinados;
- três bosses;
- conta nova e conta avançada;
- Sandbox com objetivo de comparação de torres;
- mapa personalizado depois de fechar/reabrir;
- bot desligado, sugestão e automático.

### Balanceamento

Registar por vaga:

- créditos ganhos/gastos;
- compra vs upgrade;
- dano e uptime por torre;
- inimigos que chegam à base;
- composição e counters;
- score das decisões do bot;
- duração e motivo de derrota.

Alterar números apenas depois de recolher uma baseline. Cada alteração de balanceamento deve indicar hipótese, métrica esperada e resultado.

### Critérios de saída

- nenhum bloqueador P0/P1;
- UI legível em vagas densas;
- todos os fluxos manuais registados com o template de playtest;
- bundle testado num segundo computador físico;
- conta exportada num computador e importada no outro;
- matriz de paridade sem gates críticos.

## Fase 6 - Cutover

Prioridade: **decisão explícita**

### Aprovar apenas se

- checklist de cutover completa;
- checkpoint e tag de rollback existentes;
- bundle final reproduzível;
- contas/saves com round-trip e recuperação testados;
- segundo computador validado;
- README e launchers atualizados;
- utilizador aprova explicitamente substituir o launcher principal.

### Rollback

Até essa aprovação:

- `run-game.bat` da raiz continua a abrir Haskell;
- o bundle Haskell permanece disponível;
- o Godot continua lado a lado;
- nenhum save Haskell original é removido.

## Próximo sprint recomendado

Executar exatamente nesta ordem:

1. rever e criar o checkpoint Git;
2. realizar o playtest manual de duas contas e transferência num segundo computador;
3. testar partidas completas com a grelha global 12x11 e limites 12/16/18/24/30, ajustando apenas com evidência;
4. completar cenários automáticos do Bot 2.0 por classe e boss;
5. validar resize e escala Windows;
6. criar grelha de coleção ou cards de torres, escolhendo apenas um incremento visual de cada vez;
7. correr Haskell 166/166 e Godot 211/211 mais os novos testes;
8. só depois avaliar launcher/cutover.

## Fora do âmbito imediato

- backend online e autenticação cloud;
- multiplayer;
- monetização real;
- marketplace de assets sem proveniência;
- reescrita total da simulação;
- apagar a versão Haskell.

Se for desejado login entre computadores, criar primeiro uma decisão arquitetural separada sobre backend, identidade, segurança, conflitos de save, custos e privacidade. O sistema local atual não deve ser apresentado como conta online.
