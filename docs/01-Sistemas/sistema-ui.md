# Immutable Towers - Sistema UI

Tags: #sistema #ui

Relacionadas: [[HOME]], [[estado-atual]], [[backlog-jogo]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]]

## Modulos

### Haskell/Gloss

- `app/Desenhar.hs`: composicao visual
- `app/UIComponents.hs`: paineis, botoes e `UIRect`
- `app/UIRects.hs`: geometria interativa partilhada
- `app/UIState.hs`: dados derivados para HUD e vagas
- `app/UIText.hs`: tipografia bitmap/Gloss
- `app/VisualTheme.hs`: tokens de cor semanticos
- `app/UIIcons.hs`: icones procedurais pequenos para HUD e estados
- `app/Eventos.hs`: input e bloqueio da UI
- `lib/MapGeometry.hs`: conversao mapa/ecra

### Godot

- `godot/scenes/boot/boot.tscn`: shell do menu;
- `godot/src/presentation/boot_screen.gd`: navegação, perfil, loja, ranking, ajuda e opções;
- `godot/scenes/game/vertical_slice.tscn`: HUD, mapa, arsenal, ações e overlays;
- `godot/src/presentation/vertical_slice_screen.gd`: input, HUD, bot e checkpoints;
- `godot/src/presentation/vertical_slice_view.gd`: modelos e efeitos procedurais;
- `godot/src/presentation/game_layout.gd`: escala única da grelha;
- `godot/src/presentation/menu_background.gd`: cenário animado do menu;
- `godot/scenes/ui/shop_chest_card.tscn`: modelo e animação dos baús;
- `godot/scenes/ui/shop_reward_reveal.tscn`: revelação persistente da recompensa;
- `godot/assets/generated/base_theme.tres`: tema atual.

## Layout Haskell

- espaco virtual base de 1920x1080, escalado uniformemente para a janela
- HUD e controlos no topo
- loja recolhivel na esquerda
- painel contextual recolhivel na direita
- mapa ocupa o centro sem receber cliques atraves dos paineis

## Layout Godot

- viewport virtual 1920x1080 com `canvas_items`;
- menu em duas colunas: navegação à esquerda e conteúdo à direita;
- partida com barra superior, mapa à esquerda e painel/arsenal à direita;
- catálogo-fonte 36x34 convertido no runtime para 12x11 células globais de 66 px, com rodapé para mapa e capacidade de torres;
- pausa, vitória e derrota em overlay;
- HUD e painel podem ser recolhidos;
- o layout anterior foi capturado em 1280x720, 1366x768, 1280x800 e 2560x1080; a nova grelha global foi revalidada em 1024x768, 1280x720, 1920x1080 e 2560x1080;
- 1280x720 e 1920x1080 são utilizáveis;
- a 1024x768 texto, baús e cards ficam pequenos;
- em 21:9 o grupo fica ancorado à esquerda e sobra espaço à direita;
- em 4:3 o grupo fica no topo e sobra espaço abaixo;
- a causa é a cena de partida usar offsets fixos em vez de contentores centrados/reflow;
- falta redimensionamento contínuo, escala Windows 100%/125%/150% e escala de UI configurável.

## Assets, tema e performance

- Ambos os renderers usam sobretudo modelos procedurais para mapa, torres, inimigos e HUD.
- Os BMP historicos continuam em `app/imagens/` para manter o bundle compativel, mas nao sao carregados no arranque.
- A lista `imagens` fica vazia enquanto nao existir um consumidor real de sprites; isto evita leituras duplicadas e memoria ocupada sem efeito visual.
- Quando forem introduzidos sprites, devem ser carregados uma vez num asset manager e reutilizados por Picture.
- Godot não copia os BMP por falta de proveniência/licença clara.
- O Theme Godot define tipografia, cores e estados normal/hover/pressed/focus/disabled.
- O menu usa paisagem, grelha em perspetiva, estrada, portal, inimigos, torres e projéteis procedurais em camadas.
- A loja usa modelos de baú procedurais, sem depender dos BMP históricos nem introduzir licenças desconhecidas.
- Não existe sistema de áudio real.

## Fonte de verdade

`UIRects.hs` define as areas desenhadas e interativas. O painel lateral usa `gamePanelRect` tanto no render como em `cliqueBloqueadoPelaUI`; os botoes de upgrade, especializacao, venda e limpar sao testados para permanecer dentro desse painel.

O painel esta dividido em faixas fixas:

1. mapa, capitulo e preview da proxima vaga;
2. estado da base/partida;
3. torre selecionada e comparacao do upgrade;
4. acoes.

## Estados e acessibilidade

- hover, selecionado, desativado e dinheiro insuficiente
- contraste por texto e contorno, nao apenas cor
- pausa, 1x/2x/4x, HUD e loja recolhiveis
- vitoria e derrota com fluxo proprio
- submenus com fundo animado comum e botoes `Voltar` ligados a hitboxes partilhadas

No Godot, redução de efeitos, números de dano e ecrã inteiro persistem por conta. A loja deixou de depender do `RichTextLabel` e dos cinco botões genéricos; perfil, ranking, ajuda e modos ainda reutilizam o controlador genérico.

## Hotspots Godot

- `boot_screen.gd` concentra páginas com necessidades muito diferentes;
- `vertical_slice_screen.gd` mistura input, persistência, bot e HUD;
- `vertical_slice_view.gd` concentra todos os modelos procedurais;
- a cena de partida usa offsets fixos no espaço virtual; deve passar para `Control`/`Container`, anchors e breakpoints;
- a organização interna ainda conserva nomes históricos `vertical_slice`, mas já não os apresenta ao jogador.

## Aberto prioritário

- centrar o conteúdo total e criar layouts compacto, 16:9 e 21:9;
- suportar 1024x768 sem texto/modelos ilegíveis;
- adicionar escala de UI 80-150%;
- extrair controladores próprios para coleção, perfil e ranking quando a complexidade justificar;
- cards com ícones para torres e vagas;
- grelha de coleção e detalhe de cada torre;
- modelos por nível/especialização e telegraphs;
- validação de resize e escala Windows;
- navegação completa por teclado, foco visível e remapeamento;
- contraste 4,5:1 para texto normal e canais não cromáticos para estados;
- áudio apenas depois de existir pipeline licenciado.

Ver [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 5 - UI responsiva, UX e acessibilidade|UI responsiva]] e [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 6 - Direção visual, modelos, loja, VFX e áudio|Direção visual]].
