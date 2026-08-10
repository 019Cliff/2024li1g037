# Immutable Towers - Plano de Melhoria Total - 2026-07-29

Tags: #plano #roadmap #gameplay #bot #ui #arte #qa #release

Relacionadas: [[00-Inicio/estado-atual|Estado Atual]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]], [[backlog-jogo|Backlog]], [[roadmap-atual|Roadmap Atual]]

## Objetivo

Transformar o candidato Godot num tower defense curto, legível e estratégico, em que:

- cada torre tem um papel reconhecível;
- cada mapa e modo pede decisões diferentes;
- construir, melhorar, especializar e poupar são escolhas reais;
- o bot consegue explicar e executar uma estratégia competente;
- nenhuma partida finita fica presa;
- UI, modelos, efeitos e som comunicam o estado sem depender apenas de texto ou cor;
- conta, save e release são seguros.

Este plano substitui a ordem antiga sempre que existir conflito. A evidência que o fundamenta está na [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]].

## Decisões já tomadas

- manter a grelha global **12x11**;
- manter uma torre por célula global;
- manter temporariamente os limites atuais por modo;
- continuar em Godot e preservar Haskell como referência/rollback;
- corrigir integridade e duração das partidas antes de equilibrar apenas números;
- melhorar o bot por previsão de resultados, não por acumulação de pesos;
- não começar já multiplayer, heróis, maze-building, backend cloud ou uma camada roguelite grande;
- só acrescentar uma feature diferenciadora depois de o núcleo estar estável.

## Estado de execução em 2026-07-29

- **Fase 1 concluída em código e testes:** investimento real, refund de 65%, migração conservadora, caps/tenacidade/diminishing returns e serviço puro de pontuação;
- **Fase 2 iniciada:** o gate integrado valida 45 transações e nove partidas finitas, sem arbitragem, erros de ação ou ausência prolongada de progresso;
- **Bot 3.0 em calibração:** `poupar`, defesa mínima, memória e rollout reduzido estão ligados; ainda não supera Sentinela-only em todos os mapas;
- **Campanha perfilada:** os cinco estágios usam composições distintas e vencem na baseline automática;
- **Ainda aberto:** superioridade/diversidade do bot, telemetria completa, balanceamento de Desafio/Bosses, UI responsiva e export release;
- o launcher principal continua em Haskell até aprovação explícita.

## Ordem obrigatória

```text
checkpoint
  -> economia e stalemates
  -> testes de resultado
  -> Bot 3.0
  -> campanha, modos e pontuação
  -> UI responsiva e acessibilidade
  -> modelos, loja, VFX e áudio
  -> conteúdo opcional
  -> release e cutover
```

Trabalho visual pode ser prototipado em paralelo, mas não deve bloquear nem mascarar os problemas de gameplay.

---

## Fase 0 - Proteger o estado atual

### Trabalho

1. rever o diff completo;
2. separar alterações intencionais de ficheiros temporários;
3. criar um checkpoint Git identificado como candidato Godot pré-balanceamento;
4. guardar os resultados da auditoria e o hash do ZIP;
5. confirmar que o harness `godot/tools/audit_gameplay.gd` fica fora do bundle;
6. criar uma branch curta por fase, sem misturar balanceamento, UI e release.

### Critério de saída

- é possível regressar exatamente ao candidato auditado;
- testes Haskell e Godot continuam verdes;
- nenhum save real foi incluído no repositório.

---

## Fase 1 - Integridade do gameplay

Esta é a fase prioritária. Não acrescentar torres, inimigos ou baús antes de a fechar.

### 1.1 Corrigir compra, upgrade e venda

Problema: alguns modos cobram um preço com desconto, mas a torre guarda o preço base. A venda pode devolver mais do que foi pago.

#### Implementação

- guardar `purchase_price_paid` com o custo realmente debitado;
- guardar investimento pago por upgrade e especialização;
- calcular o reembolso sobre o investimento acumulado real;
- escolher uma taxa explícita de reembolso, inicialmente **65%**;
- arredondar sempre de forma previsível;
- migrar saves antigos com uma estimativa conservadora;
- mostrar no painel `investido` e `recebes ao vender`;
- impedir que qualquer sequência comprar -> melhorar -> vender produza créditos.

#### Testes obrigatórios

- todas as nove torres;
- todos os modos;
- todos os níveis;
- duas especializações;
- compra e venda imediata;
- compra, vários upgrades e venda;
- save/load entre compra e venda;
- propriedade: `saldo_final <= saldo_inicial` sem recompensa de vaga.

### 1.2 Resolver controlo infinito e stalemates

Problema: gelo, eletricidade e medo podem impedir progresso durante 15 minutos ou mais.

#### Modelo recomendado

- `slow`: reduz velocidade, mas nunca abaixo de um mínimo;
- `stun`: hard CC curto, com limite de duração acumulada;
- `fear`: recuo curto em inimigos normais; slow/interrupt em bosses;
- reaplicação refresca até um teto, não soma sem limite;
- bosses e elites recebem tenacidade;
- hard CC repetido aplica diminishing returns por janela temporal;
- o estado mostra duração e resistência restante sem depender apenas de cor.

Valores iniciais para teste, não finais:

| Regra | Inimigo normal | Elite | Boss |
|---|---:|---:|---:|
| Velocidade mínima sob slow | 25% | 35% | 50% |
| Multiplicador de hard CC | 100% | 60% | 25% |
| Teto de stun contínuo | 1,5 s | 1,0 s | 0,5 s |
| Janela de diminishing returns | 6 s | 8 s | 10 s |

#### Proteção de produto

- watchdog apenas de desenvolvimento deteta uma vaga sem dano, avanço ou mudança de fase;
- boss ganha tenacidade/enrage gradual quando o encontro não progride;
- não matar nem teletransportar automaticamente o inimigo em produção;
- uma vaga finita tem sempre um caminho de resolução: morte dos inimigos ou fuga para a base.

#### Testes obrigatórios

- apenas Glaciar em História;
- apenas Pânico em Bosses;
- Glaciar + Tesla com cadência elevada;
- boss com escoltas;
- 1x, 2x e 4x;
- save/load durante um estado;
- campanha completa com teto de tempo.

### 1.3 Corrigir a pontuação

Problema: a fórmula atual recompensa tempo decorrido e número de torres.

#### Fórmula proposta

```text
pontuação =
  base_por_vagas_e_ameaças
  + bónus_vida_base
  + bónus_tempo_decrescente
  + bónus_eficiência_com_teto
  + multiplicador_modo
  + multiplicador_contratos
```

Regras:

- esperar nunca aumenta a pontuação;
- construir uma torre sem utilidade nunca dá pontos;
- vender/comprar repetidamente não altera o score;
- tempo é comparado com um par por mapa/modo, não com um valor universal;
- score é inteiro, determinístico e reproduzível depois de save/load.

### Critério de saída da Fase 1

- zero arbitragem em toda a matriz económica;
- todas as vagas finitas terminam ou derrotam a base dentro do teto definido;
- nenhum boss fica permanentemente controlado;
- score não aumenta por idle nem spam;
- novos testes falham nas regras antigas e passam nas novas;
- 250 checks, incluindo os novos invariantes e a curva de recompensas do Infinito, continuam verdes.

---

## Fase 2 - Harness de resultado e balanceamento observável

Os testes unitários provam regras locais. Esta camada deve provar que o jogo chega a resultados aceitáveis.

### Harness permanente

Evoluir `godot/tools/audit_gameplay.gd` para produzir JSON/CSV opcional com:

- mapa, modo, seed, arsenal e versão do balanceamento;
- vitória/derrota/stalemate;
- duração;
- vaga final;
- vida da base;
- créditos ganhos, gastos e em caixa;
- torres construídas por identidade;
- upgrades e especializações;
- dano, controlo e eliminações por torre;
- leaks por classe;
- tempo sem progresso;
- ações e razões do bot.

### Cenários fixos

- conta nova com Sentinela;
- arsenal completo;
- cada torre isolada, marcado como teste de perfil e não necessariamente de vitória;
- composições de duas e três torres;
- cinco mapas em História;
- três bosses;
- Desafio;
- marcos 4, 6, 9, 12 e 18 do Infinito;
- limites de torre atingidos;
- economia inicial baixa, média e alta.

### Targets provisórios

Estes valores servem para detetar regressões; devem ser ajustados depois de playtests humanos:

| Métrica | Alvo inicial |
|---|---|
| História inicial | 8-14 min |
| História avançada | 10-18 min |
| Boss | 6-12 min |
| Vaga normal sem progresso | menos de 20 s |
| Boss sem progresso | menos de 30 s |
| Torres distintas usadas pelo bot completo em cinco mapas | pelo menos 6/9 |
| Mapas em que só Sentinela domina a estratégia completa | 0 |
| p95 do stress test de simulação | menos de 8 ms |

### Critério de saída

- o harness corre sem UI e termina com exit code não-zero numa regressão;
- resultados de referência ficam versionados;
- alterações de balanceamento mostram antes/depois;
- seeds e versões tornam uma falha reproduzível.

---

## Fase 3 - Bot 3.0

### Objetivo

O bot deve escolher a ação que prevê melhor resultado, respeitar a economia futura e explicar a decisão em linguagem curta.

### Arquitetura

Separar o bot em quatro peças puras:

1. `BotContext`: estado reduzido da partida;
2. `CandidateGenerator`: ações legais e relevantes;
3. `ActionEvaluator`: rollout curto e score de resultado;
4. `BotMemory`: objetivo, compromisso e resultado anterior.

### Ciclo de decisão

1. resumir pressão atual e duas vagas;
2. verificar defesa mínima;
3. gerar 5-12 candidatos:
   - construir numa das melhores células;
   - melhorar uma torre de alto impacto;
   - escolher especialização;
   - vender apenas em recuperação planeada;
   - poupar para um objetivo concretamente atingível;
4. clonar um estado reduzido;
5. simular uma janela curta;
6. medir leaks, tempo para matar, vida prevista, rendimento e cobertura;
7. eliminar planos que deixem a defesa sem capacidade de matar;
8. aplicar histerese para não trocar de objetivo a cada ciclo;
9. executar pela mesma API do jogador;
10. registar previsão e resultado real.

### Regras de segurança

- nunca poupar sem rendimento futuro provável;
- nunca poupar enquanto a defesa mínima falha;
- não preencher automaticamente o limite antes de comparar upgrades;
- limitar redundância de controlo;
- reservar dano suficiente antes de comprar suporte;
- evitar vender uma peça crítica no meio da vaga;
- não conhecer inimigos além da informação disponível ao jogador;
- manter determinismo por seed.

### Explicação ao jogador

Formato:

```text
Melhorar Braseiro N2
Evita ~3 fugas na vaga atual e custa menos 28 que uma nova torre.
Alternativas: Sentinela em C7; poupar para Tesla.
```

O estado `poupar` é uma decisão válida, não um erro/idle.

### Perfis opcionais

Só depois do bot base funcionar:

- `Seguro`: maximiza vida da base;
- `Equilibrado`: valor predefinido;
- `Agressivo`: aceita risco por tempo/score.

Não criar personalidades diferentes antes de existir uma política competente.

### Aceitação

- arsenal completo supera ou iguala Sentinela-only nos cinco mapas;
- termina Cruzamento Solar;
- não fica a poupar sem rendimento;
- usa pelo menos seis identidades ao longo da matriz completa;
- escolhe upgrades antes do cap quando são superiores;
- responde corretamente a rápido, blindado, regenerador, dispersor, protegido, elite e bosses;
- cada ação mutável mantém checkpoint imediato;
- orçamento de decisão não causa stutter visível.

---

## Fase 4 - Campanha, modos e balanceamento

### 4.1 Perfil de dificuldade

Criar um `RunDifficultyProfile` puro derivado de:

- capítulo;
- estágio;
- mapa;
- modo;
- contratos/modificadores ativos;
- nível de conta apenas quando a regra for explícita.

O perfil controla dentro de limites:

- vida, ataque e velocidade;
- composição e intervalos de spawn;
- quantidade e ordem de classes;
- economia inicial e recompensa;
- boss/escoltas;
- metas de tempo e score.

Não escalar apenas vida. Misturar pressão, velocidade, counters, entradas e economia.

### 4.2 Identidade dos cinco mapas

| Mapa | Identidade proposta | O que deve testar |
|---|---|---|
| Planície Serena | onboarding e curvas largas | fundamentos, upgrades e alvo primeiro |
| Garganta de Pedra | zonas de contacto curtas e fortes | posicionamento preciso e dano concentrado |
| Lago Fraturado | ilhas e cobertura dividida | alcance, escolha entre flancos e economia |
| Cruzamento Solar | múltiplos pontos de pressão | adaptação e cobertura sem controlo infinito |
| Bastião Espiral | contacto longo com escalada tardia | composições, especializações e bosses |

Cada mapa deve ter:

- uma decisão espacial distinta;
- uma composição própria de vagas;
- um momento visual memorável;
- uma baseline de duração e dificuldade;
- posições fortes, mas nenhuma célula obviamente correta para todas as torres.

### 4.3 Modos

- **História:** campanha curada e aprendizagem progressiva;
- **Infinito:** escalada legível, mutadores anunciados e score comparável;
- **Desafio:** uma restrição clara, não apenas menos dinheiro;
- **Bosses:** encontros curtos por fases e telegraphs;
- **Livre:** laboratório sem progressão competitiva.

Exemplos de Desafio:

- arsenal limitado a quatro torres;
- upgrades custam mais, mas venda devolve mais;
- inimigos rápidos e menos numerosos;
- mapa com capacidade reduzida;
- sem hard CC, com recompensa superior.

Usar uma ou duas regras por desafio. Evitar combinações opacas.

### 4.4 Torres

Cada torre precisa de:

- papel principal;
- inimigos contra os quais é forte;
- fraqueza real;
- parceiro/sinergia;
- razão para upgrade;
- duas especializações legíveis;
- alvo e DPS efetivo mostrados no painel.

Direção de balanceamento:

- reduzir a eficiência universal da Sentinela, não apenas aumentar o preço sem medir;
- separar claramente dano, suporte e controlo;
- garantir que Glaciar ajuda a matar, mas não substitui dano;
- garantir que Pânico reposiciona pressão sem bloquear a partida;
- tornar Venenoide relevante contra vida alta/regeneração;
- dar à Tesla valor anti-enxame inequívoco;
- dar ao Impacto burst/armadura sem competir em tudo;
- dar ao Solar uma sinergia ofensiva mensurável;
- fazer Tempestade parecer endgame sem ser requisito de vitória.

### 4.5 Inimigos e bosses

- introduzir counters um de cada vez na campanha;
- mostrar informação ao selecionar/hover;
- usar silhueta, animação e ícone de estado além da cor;
- telegraph antes de aceleração, escudo, aura ou regen;
- fases de boss mudam comportamento, não só números;
- escoltas criam uma decisão diferente do corpo principal;
- resistências nunca tornam uma composição completamente incapaz de progredir sem aviso.

### Critério de saída

- os cinco estágios não repetem a mesma tabela de dez vagas;
- dificuldade cresce apesar de rotas mais longas;
- Desafio e Bosses têm baselines vencíveis;
- cada torre tem pelo menos um cenário em que é a melhor escolha e um em que não é;
- playtests humanos não identificam uma estratégia universal.

---

## Fase 5 - UI responsiva, UX e acessibilidade

### 5.1 Estrutura responsiva

Substituir offsets fixos por `Control`, `Container`, anchors e margens semânticas.

Breakpoints propostos:

- **compacto/4:3:** mapa centrado, painel alterna por tabs/drawer;
- **16:9:** mapa e painel lado a lado;
- **21:9:** grupo completo centrado; espaço extra vira margem, nunca vazio unilateral;
- **janela pequena:** UI reduz até ao mínimo legível e depois reorganiza.

Suporte alvo:

- desenho base: 1920x1080;
- mínimo funcional: 1024x768;
- 1280x720, 1366x768, 1600x900 e 1920x1080;
- 2560x1080 e 2560x1440;
- escala Windows 100%, 125% e 150%;
- redimensionamento contínuo no menu, loja e partida.

Adicionar escala de UI configurável de **80% a 150%**, independente da escala do mundo.

### 5.2 Hierarquia da partida

Ordem visual:

1. mapa e ameaça;
2. base, vaga e créditos;
3. ação atual/torre selecionada;
4. arsenal;
5. detalhes avançados.

Melhorias:

- cards de torre com ícone, custo, papel e estado;
- preview de vaga por ícones, quantidades, resistências e perigo;
- painel contextual com comparação antes/depois;
- aviso de capacidade perto do arsenal, sem dominar o HUD;
- feedback inequívoco de célula válida, inválida, ocupada e limite atingido;
- informação detalhada de inimigo ao selecionar/hover;
- atalhos apresentados na própria UI;
- tooltips curtos e consistentes.

### 5.3 Sistema de cor

Paleta inicial a validar em contraste:

| Token | Cor proposta | Uso |
|---|---|---|
| `bg.deep` | `#08111F` | fundo |
| `surface.base` | `#101D2E` | painéis |
| `surface.raised` | `#172A40` | cards |
| `accent.gold` | `#F4C95D` | progresso/recompensa |
| `accent.cyan` | `#59D9FF` | seleção/informação |
| `state.success` | `#73D99A` | válido/sucesso |
| `state.warning` | `#FFB454` | risco |
| `state.danger` | `#FF6B6B` | dano/erro |
| `text.primary` | `#F2F6FA` | texto principal |
| `text.secondary` | `#B6C4D2` | texto secundário |

Regras:

- cor nunca é o único canal;
- usar ícone, forma, padrão, contorno e texto;
- texto normal procura pelo menos contraste 4,5:1;
- foco de teclado tem contorno próprio;
- estados disabled mantêm legibilidade;
- perigo, resistência e afinidade têm símbolos consistentes.

### 5.4 Acessibilidade

- navegação completa por teclado;
- ordem de foco previsível;
- remapeamento de teclas;
- tamanho de texto/UI;
- redução de movimento;
- redução de flashes e shake;
- números de dano opcionais;
- velocidade 1x/2x/4x com estado visível;
- pausa em perda de foco configurável;
- legendas/texto para cues de áudio importantes;
- filtros/modos de daltonismo só depois de os canais não cromáticos existirem;
- confirmar alvos de clique confortáveis e não sobrepostos.

### Critério de saída

- nenhuma sobreposição nas resoluções alvo;
- jogo centrado em 4:3, 16:9 e 21:9;
- texto e ações principais legíveis a 1024x768;
- fluxo menu -> conta -> loja -> jogo -> pausa -> resultado funciona só por teclado;
- reduced effects cobre menu, loja, combate e recompensa.

---

## Fase 6 - Direção visual, modelos, loja, VFX e áudio

### 6.1 Gramática visual das torres

Todas as torres usam três camadas:

1. **fundação:** família/ocupação;
2. **núcleo:** energia e identidade;
3. **arma:** papel mecânico e direção do ataque.

Silhuetas propostas:

| Torre | Leitura visual |
|---|---|
| Sentinela | balista/lança frontal com lente de mira |
| Glaciar | cristal dividido e anel refrigerante |
| Braseiro | fornalha, chaminé e boca de fogo |
| Pânico | olho/orbe suspenso com placas radiais |
| Venenoide | dois frascos, tubos e agulha |
| Tesla | bobina alta com dois ou três terminais |
| Impacto | canhão baixo, pesado, com recuo visível |
| Solar | espelhos inclinados e halo concentrador |
| Tempestade | coroa híbrida, bobina e núcleo climático |

Progressão por nível:

- N1: forma limpa e função básica;
- N2: núcleo ativo;
- N3: arma maior ou segundo mecanismo;
- escolha: bifurcação estrutural inequívoca;
- níveis finais: peças, emissão e animação próprias, não apenas escala.

Especializações:

- ramo de potência usa massa, núcleo concentrado e ataques mais marcados;
- ramo de cadência usa mecanismos múltiplos, partes móveis e ritmo rápido;
- a silhueta deve ser reconhecível mesmo sem cor e à distância normal.

### 6.2 Inimigos

- Básico: corpo simples e marcha regular;
- Rápido: inclinado, pernas/rodas longas e trail curto;
- Tanque: largo, baixo e pesado;
- Blindado: placas sobrepostas;
- Regenerador: núcleo pulsante;
- Dispersor: forma separada/radial;
- Protegido: escudo frontal visível;
- Elite: combinação maior com acento único;
- bosses: silhueta ocupa mais espaço, telegraph próprio e transição de fase.

Criar uma folha de modelo com frente, perfil, escala no mapa e estados principais antes de implementar cada família.

### 6.3 Loja de baús

Objetivo: o baú é o protagonista; preço e estado são claros; a animação nunca esconde a decisão.

Layout:

- um baú em destaque ou carousel de três cards largos;
- pedestal/sombra e espaço suficiente para a silhueta;
- nome, pool e preço juntos;
- saldo sempre visível;
- estado bloqueado/insuficiente abaixo do modelo, nunca por cima;
- botão com verbo e custo;
- coleção e detalhe acessíveis a partir da loja.

Modelos:

- Madeira: tábuas, cintas, fecho simples e abertura leve;
- Cristal: facetas, núcleo frio e refração;
- Imperial: metal, cantos, brasão e luz dourada.

Sequência:

1. confirmação opcional;
2. transação persistida;
3. antecipação curta;
4. tampa abre;
5. luz/partículas;
6. silhueta da torre;
7. nome, nova/duplicada e compensação;
8. recolher;
9. coleção atualizada.

Regras:

- duração total normal 1,5-2,5 s;
- reduced effects usa dissolve/fade curto;
- pode saltar depois da primeira visualização;
- fechar/reabrir recupera diretamente a recompensa pendente;
- sem texto a atravessar o baú;
- probabilidades e pity só aparecem depois de uma decisão de produto e testes do domínio.

### 6.4 Menu inicial

- iluminar ligeiramente a batalha procedural atrás do conteúdo;
- reservar uma zona calma para os painéis;
- criar ciclos de 12-20 s com portal -> inimigos -> torre -> impacto -> dissipação;
- variar pequenas formações sem aleatoriedade caótica;
- usar parallax subtil e não mover texto/painéis;
- reduced effects mantém composição viva com emissões estáticas;
- em ultrawide, estender cenário e centrar o conteúdo.

### 6.5 VFX

- linguagem distinta para impacto direto, área, burn, slow, poison e electricidade;
- telegraph de boss precede sempre o efeito;
- limitar partículas por torre/inimigo e agregar em alta densidade;
- prioridade: ameaça e dano à base > ataque normal > decoração;
- flashes curtos, sem cobrir barras ou rota;
- opção de intensidade de efeitos.

### 6.6 Áudio

Introduzir apenas com assets próprios/licenciados e proveniência registada:

- música de menu;
- camada calma e camada de pressão em gameplay;
- build, upgrade, sell e erro;
- assinatura curta por família de torre;
- entrada de vaga, elite, boss e dano na base;
- recompensa de baú nova/duplicada;
- sliders reais de master, música, efeitos e UI;
- cues importantes também visuais.

### Critério de saída

- nove torres reconhecíveis sem ler o nome;
- especializações distinguíveis à escala normal;
- classes e estados dos inimigos usam forma/ícone além de cor;
- loja legível a 1024, 16:9 e 21:9;
- nenhuma animação pode duplicar ou perder uma transação;
- orçamento de VFX mantém o frame-time definido.

---

## Fase 7 - Progressão, conta e retenção saudável

### Progressão

- recompensar conclusão, domínio e variedade, não grind infinito;
- revelar uma nova mecânica de cada vez;
- mostrar requisitos e recompensa dos modos;
- desbloqueios têm apresentação própria e podem ser revistos na coleção;
- evitar upgrades meta que sejam obrigatórios para corrigir balanceamento;
- usar mastery cosmética ou lateral só depois da campanha base estar equilibrada.

### Coleção

- grelha das nove torres;
- estado bloqueada/desbloqueada;
- papel, counters, stats e especializações;
- preview dos modelos por nível;
- origem de desbloqueio;
- campo de treino futuro, se o editor não cobrir essa necessidade.

### Contas locais

- concluir playtest completo por UI;
- mostrar estatísticas como inteiros;
- testar duas contas isoladas;
- testar export/import num segundo computador;
- mostrar localização e data do último backup;
- fluxo explícito para substituir/importar como nova;
- preservar `.tmp`, `.bak`, versionamento e migração.

### Login/cloud opcional

Não chamar “login” ao seletor local. Se for exigido progresso em qualquer computador, tratar como projeto separado:

- backend autenticado;
- palavra-passe ou fornecedor externo;
- recuperação de conta;
- tokens seguros;
- save remoto versionado;
- estratégia de conflito local/remoto;
- eliminação/exportação de dados;
- modo offline e reconciliação;
- política de privacidade e operação do serviço.

Só avançar depois de definir custo de operação, plataforma e necessidade real.

---

## Fase 8 - Uma feature diferenciadora, apenas depois do núcleo

### Recomendação: Contratos de Defesa

Antes de cada partida, o jogador escolhe zero a três modificadores claros e recebe um multiplicador de score/recompensa.

Exemplos:

- `Marcha Forçada`: inimigos +15% rápidos, +12% score;
- `Arsenal Curto`: máximo de quatro tipos de torre, +18% score;
- `Fortificação`: elites ganham armadura, +15% score;
- `Linha Frágil`: base começa com menos vida, +20% score;
- `Escassez`: menos recompensa por kill, +22% score.

Porque encaixa:

- reutiliza sistemas existentes;
- cria variedade sem dezenas de mapas;
- dá propósito ao bot e aos counters;
- torna score comparável;
- inspira-se em dificuldade opt-in, mas mantém identidade própria.

Regras:

- impacto e recompensa sempre visíveis;
- combinações incompatíveis são bloqueadas;
- seeds e contratos entram no resultado/ranking;
- História principal continua jogável sem contratos;
- máximo inicial de seis contratos bem testados.

### Feature secundária possível: loadout

Depois de as nove torres estarem equilibradas, testar um loadout de cinco identidades antes da partida. Pode aumentar a decisão estratégica e tornar a coleção relevante, mas só deve avançar se não prejudicar onboarding nem tornar counters obrigatórios invisíveis.

### Features a adiar

- multiplayer;
- guildas;
- heróis controláveis;
- labirinto construído pelo jogador;
- dezenas de raridades;
- loot com stats aleatórios;
- battle pass;
- mundo aberto;
- cloud obrigatória.

---

## Fase 9 - Arquitetura, CI, desempenho e release

### Extração orientada às features

- controlo/estados -> resolvedor próprio;
- dificuldade/campanha -> `RunDifficultyProfile`;
- pontuação -> serviço puro;
- bot -> contexto, candidatos, rollout e memória;
- `vertical_slice_screen.gd` -> input, HUD e sessão;
- `boot_screen.gd` -> navegação, loja, coleção, perfil e ranking;
- `vertical_slice_view.gd` -> renderers por família;
- testes -> suites por domínio + campanhas.

Não reescrever tudo. Extrair quando uma mudança concreta tocar no hotspot.

### CI

Pipeline mínima:

1. validação do catálogo;
2. testes Haskell;
3. testes Godot;
4. import headless;
5. auditoria rápida de economia e stalemate;
6. campanhas determinísticas reduzidas;
7. benchmark com limite de regressão;
8. export release;
9. hash e artefacto.

### Desempenho

- separar warmup, p95 estável e pior frame;
- medir com render/VFX, não só simulação;
- cache de features estáticas do bot por mapa;
- pooling apenas onde profiling provar custo;
- limites explícitos de partículas e números de dano;
- benchmark de decisão do bot;
- evitar otimização sem perfil.

### Release Windows

- instalar templates oficiais compatíveis com a versão Godot;
- criar preset de produção;
- usar `--export-release`;
- não copiar o executável completo do editor;
- confirmar que o título não contém `(DEBUG)`;
- gerar ZIP numa pasta limpa;
- testar sem Godot/Haskell/Cabal;
- validar SmartScreen/assinatura conforme o destino de distribuição;
- preservar licenças necessárias;
- testar conta e backup num segundo computador;
- só então decidir o cutover do launcher.

### Critério de saída

- CI verde num checkout limpo;
- pacote usa template release;
- testes e ferramentas não entram no ZIP;
- nenhuma dependência de desenvolvimento é necessária;
- matriz de resolução, input, conta e saves aprovada;
- checklist de cutover aprovado explicitamente.

---

## Próximos 12 tickets, na ordem recomendada

- [x] corrigir preço pago e refund;
- [x] criar matriz automática de arbitragem;
- [x] implementar caps/diminishing returns de controlo;
- [x] criar testes anti-stalemate;
- [x] extrair e corrigir score;
- [x] transformar o harness em gate de resultado;
- [x] corrigir `save` do bot como decisão válida;
- [x] adicionar defesa mínima e poupança atingível;
- [x] implementar rollout curto do Bot 3.0;
- [x] criar `RunDifficultyProfile`;
- [x] dar vagas próprias aos cinco estágios;
- [ ] converter a cena de gameplay para layout centrado/responsivo.

Depois destes tickets, repetir a auditoria antes de avançar para modelos e conteúdo.

## Métricas de produto a observar em playtest

- taxa de vitória por mapa/modo;
- duração por partida e por vaga;
- vida restante da base;
- distribuição de torres construídas;
- frequência de upgrades vs. novas torres;
- células mais usadas;
- créditos não gastos no fim;
- causas de fuga;
- tempo em pausa/velocidade;
- clareza de estados e counters;
- abandono/restart;
- satisfação com bot/sugestão;
- erros de conta, save e recompensa.

Guardar localmente apenas dados agregados e sem dados pessoais, com opção clara. Não criar telemetria online sem consentimento e política.

## Definition of Done global

O candidato só está pronto para substituir o Haskell quando:

- economia não permite lucro circular;
- nenhuma partida finita fica presa;
- bot completo é melhor do que a estratégia Sentinela-only;
- campanha tem curva e identidade por mapa;
- score recompensa competência, não demora;
- os cinco modos têm baselines testadas;
- UI funciona em 4:3, 16:9 e 21:9;
- teclado e opções de acessibilidade cobrem o fluxo principal;
- modelos e efeitos comunicam função/estado à distância;
- loja é clara, segura e recuperável;
- contas e backups passam num segundo computador;
- pacote Windows é um export release real;
- suites, harness, benchmark e checklist estão verdes;
- cutover recebe aprovação explícita.
