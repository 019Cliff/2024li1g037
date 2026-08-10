# Immutable Towers - Changelog Jogavel

Tags: #changelog #qa

Registo curto das mudancas com impacto real na experiencia de jogo.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[backlog-jogo]]

## 2026-07-30

### Recompensa do Infinito

- o modo Infinito passa a atribuir gemas mesmo quando termina em derrota;
- o valor depende da vaga alcançada e cresce com retornos decrescentes;
- a curva foi recalibrada para 2,5x: 3 gemas na vaga 1, 70 na 20, 125 na 50 e limite de 188 na vaga 120;
- a melhor vaga é guardada por conta e usada no ranking do modo;
- o ecrã final mostra explicitamente as gemas recebidas;
- regra isolada em `run_rewards.gd` e coberta por testes de valores, monotonia, limite e persistência;
- suite Godot expandida para 250 checks, mantendo o gate de 45 transações e nove partidas finitas.

## 2026-07-28

### Espaço e decisões de construção

- o catálogo mantém mapas-fonte 36x34, mas o jogo Godot agrega cada bloco 3x3 numa grelha global 12x11;
- a célula global mede 66 px e escala em conjunto relva, estrada, asfalto, água, torre, inimigo, portal, base e obstáculo;
- cada torre ocupa uma célula grande, com preview e fundação nessa mesma unidade;
- alcance, movimento, auras e chegada à base convertem distâncias por `1/3`, evitando alterar a escala física por engano;
- cada modo ganhou capacidade própria: 18 História, 24 Infinito, 12 Desafio, 16 Bosses e 30 Livre;
- o contador de torres está sempre visível junto ao mapa;
- vender liberta capacidade e partidas antigas migram coordenadas sem perder torres acima do limite ou convergentes;
- o bot deixa de propor construções sem espaço e passa a considerar upgrades ou poupança.

### Loja e menu

- a loja deixou de ser uma lista textual e ganhou três modelos procedurais de baú;
- Madeira, Cristal e Imperial têm materiais, cores, brilho, hover, flutuação e abertura próprios;
- recompensas novas e duplicadas têm revelação animada e compensação visível;
- a fusão Tempestade mostra requisitos presentes/em falta;
- animações respeitam a opção de efeitos reduzidos e a transação é guardada antes da apresentação;
- o menu ganhou paisagem em camadas, estrada sinuosa, portal, inimigos, torres, projéteis e partículas animadas.

### Qualidade

- suite Godot expandida para 211 checks;
- menu e loja mantêm a validação anterior; a grelha global foi validada numa janela real a 1280x720 e 1920x1080;
- bundle Windows reconstruído com os novos componentes.

## 2026-07-13

### Loja e contas locais - primeira fase

- dominio transacional da loja separado em `ShopSystem.hs`, com recompensas explicitas e compensacao real de duplicados;
- cartoes de baus proceduralmente desenhados e clicaveis no ecrã de loja meta;
- tipos versionados de conta e armazenamento recuperavel preparados em `AccountTypes.hs`, `AccountSystem.hs` e `AccountStorage.hs`;
- testes de compras, fusao, nomes e independencia de contas adicionados.

### Bot estrategico - primeira fase

- adicionada decisao pura contextual em `BotStrategy.hs`;
- assistente passa a recomendar por cobertura de rota, resposta a ameacas e ganho por credito;
- aplicador valida terreno, saldo e custo de upgrade;
- adicionados testes de determinismo e economia.

### Direcao visual

- criada uma fonte de verdade de cores em `VisualTheme.hs`
- adicionados icones procedurais de base, creditos, vaga, velocidade, dano, alcance e aviso
- HUD passou a combinar simbolo e texto nos indicadores principais

### Ondas, inimigos e UI

- oito classes normais de inimigos e tres bosses com modelos proprios
- armadura, resistencias, escudo, regeneracao e fases simples
- Guardiao protege aliados; Ruptura cria uma zona que enfraquece torres
- vagas declarativas por grupos e tres mutadores combinaveis no infinito
- preview da proxima vaga no painel lateral
- corrigida a chegada a base em velocidades altas, incluindo inimigos que atravessam a base entre frames
- editor impede alteracoes que bloqueiem o caminho
- painel lateral reorganizado e alinhado com a hitbox real
- testes antigos limpos de instancias orfas e funcoes parciais
- suite atual com 166 testes a passar
- submenus passaram a usar `Voltar` com retorno por rato e teclado
- ecras de Opcoes, Loja, Perfil, Ranking e Ajuda ficaram com espacamento consistente

### Torres e progressao

- identidade explicita por torre construida, sem depender do tipo de projetil
- loja, painel e modelos ligados a uma configuracao unica por `TowerId`
- niveis maximos diferentes por torre
- painel mostra nivel, DPS aproximado, prioridade e refund
- formas e cores base distintas para as nove torres

### Saves e qualidade

- save de partida versionado com identidade, nivel e especializacao
- migracao automatica de saves antigos
- migracao e identidade cobertas por testes automaticos

## 2026-07-09

### UI e layout

- loja movida do rodape para sidebar lateral esquerda
- barra inferior aliviada para devolver area util ao mapa
- hitboxes da UI alinhadas com o render para bloquear cliques no mapa por baixo

### Torre e upgrade

- preview de upgrade adicionado ao painel lateral
- painel da torre com mais contexto: raridade, efeito, valor de venda e comparacao mais completa
- modelos das torres com melhor progressao visual por poder e por tier aproximado

### Combate

- bug de inimigos presos apos certos efeitos corrigido com separacao entre velocidade base e velocidade efetiva
- impacto visual de disparo reforcado
- numeros flutuantes simples de dano adicionados

### Documentacao

- vault do Obsidian estruturado com Home, estado atual, backlog, feedback e notas tecnicas por sistema

## Como usar esta nota

Quando houver uma nova ronda de alteracoes:

1. adicionar uma nova data
2. listar apenas mudancas sentidas pelo jogador ou pelo tester
3. evitar meter detalhes internos de implementacao aqui
