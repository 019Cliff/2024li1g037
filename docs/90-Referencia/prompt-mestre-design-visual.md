# Prompt Mestre — Direção visual, modelos e UI do Immutable Towers

Tags: #prompt #design #ui #arte #roadmap

Relacionadas: [[HOME]], [[estado-atual]], [[roadmap-atual]], [[backlog-jogo]], [[sistema-ui]], [[sistema-torres]], [[sistema-inimigos]]

> Estado: especificação histórica orientada ao renderer Haskell/Gloss. Não executar diretamente sobre o candidato Godot. A direção atual está na [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 5 - UI responsiva, UX e acessibilidade|UI]] e na [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 6 - Direção visual, modelos, loja, VFX e áudio|Direção visual]].

> Copiar a secção “Prompt” para o modelo/agente que vai executar a próxima ronda. Este documento é uma especificação de implementação, não apenas uma lista de ideias.

## Diagnóstico de origem

- O jogo usa Haskell, Gloss e desenho procedural 2D num espaço virtual de 1920x1080.
- A jogabilidade e os dados já têm identidades explícitas de torres, classes de inimigos, bosses, upgrades e especializações.
- O estilo procedural atual é uma boa base, mas a paleta está repetida por vários módulos e falta uma fonte de verdade visual.
- `Desenhar.hs` concentra demasiadas responsabilidades e torna a evolução gráfica arriscada.
- `UIText.hs` usa `Graphics.Gloss.Text`, converte texto para maiúsculas e estima largura pelo número de caracteres; não é uma fonte bitmap real.
- Os BMP carregados misturam ilustração cartoon, clipart, torre 3D limpa e portal de fantasia detalhado. Além disso, a jogabilidade atual usa sobretudo modelos procedurais, deixando vários assets carregados sem uso real.
- O contraste do texto principal e secundário é bom, mas a borda atual do painel tem cerca de 2.16:1 contra o fundo e o vermelho atual cerca de 3.67:1, insuficiente para alguns usos informativos pequenos.
- O vault já identifica como aberto: playtest multirresolução, ícones de vaga, barra de progresso, tooltips/almanac, modelos de upgrades altos, telegráficos de bosses e redução de efeitos.

## Prompt

Trabalha diretamente no repositório **Immutable Towers** como diretor de arte técnica, UI/UX designer de jogos e programador Haskell sénior. Faz uma grande revisão visual coesa e jogável, compatível com Gloss e com a arquitetura atual. O objetivo não é copiar outro tower defense nem transformar o projeto noutro motor: é fazer o jogo parecer uma versão deliberada, consistente e legível de si próprio.

### 1. Regras de trabalho

Antes de editar:

1. lê todo o vault em `docs/`, especialmente `estado-atual`, `backlog-jogo`, `sistema-ui`, `sistema-torres` e `sistema-inimigos`;
2. inspeciona `Desenhar.hs`, `UIComponents.hs`, `UIRects.hs`, `UIText.hs`, `MenuComponents.hs`, `TowerSystem.hs`, `EnemySystem.hs`, `UIState.hs` e os BMP em `app/imagens`;
3. executa `cabal build` e `cabal test` e regista o baseline;
4. captura ou observa o jogo real antes de redesenhar, se o ambiente permitir;
5. preserva todas as regras de gameplay, saves, hitboxes e os 166 testes do baseline atual, exceto quando a alteração visual exigir dados derivados puramente de apresentação.

Não faças apenas mockups. Implementa as melhorias no jogo, testa-as e deixa o projeto compilável depois de cada fase. Não introduzas uma biblioteca gráfica pesada nem uma framework UI externa sem uma justificação forte.

### 2. Direção artística alvo

Usa a identidade **fantasia defensiva com tecnologia rúnica artesanal**:

- formas simples, robustas e reconhecíveis em vista superior;
- pedra escura, metal envelhecido, madeira e núcleos elementais luminosos;
- interface verde-floresta escura com dourado envelhecido;
- mapa ligeiramente menos saturado do que torres, inimigos, projéteis e objetivos;
- detalhes luminosos reservados para informação, poder e perigo;
- contornos e sombras consistentes para separar entidades do terreno;
- sensação cartoon estilizada e limpa, sem realismo fotográfico e sem misturar clipart com arte procedural.

Hierarquia visual obrigatória, da maior para a menor prioridade:

1. perigo imediato e inimigos perto da base;
2. projéteis, impactos e estados relevantes;
3. torres selecionadas, alcance e ações disponíveis;
4. caminho, portal e base;
5. economia, vaga e HUD;
6. decoração.

Se um efeito ou decoração competir visualmente com inimigos, caminho ou UI, reduz a sua saturação, opacidade ou frequência.

### 3. Sistema visual centralizado

Cria uma fonte de verdade, por exemplo `VisualTheme.hs`, para remover cores semânticas repetidas de `Desenhar.hs`, `UIComponents.hs` e `MenuComponents.hs`.

O tema deve expor, no mínimo:

- canvas/fundo;
- superfície de painel e superfície elevada;
- borda normal, borda ativa e divisores;
- texto principal, secundário e desativado;
- acento principal;
- sucesso, aviso, erro e informação;
- seleção, hover e foco;
- cores de vida alta, média e crítica;
- cores de raridade;
- cores de efeitos elementais;
- cores de terreno.

Usa como ponto de partida, ajustando após teste de contraste:

| Token | Cor sugerida |
|---|---|
| Fundo | `#121C18` |
| Painel | `#161D19` |
| Painel elevado | `#202A23` |
| Borda | `#6B806B` |
| Texto | `#E5E9DF` |
| Texto secundário | `#AAB5A3` |
| Dourado/acento | `#E2C25F` |
| Sucesso | `#8FC176` |
| Aviso | `#F0C35A` |
| Erro | `#E66A5E` |
| Informação | `#79AFC6` |

Não uses estas cores cegamente. Mede contraste e mantém texto normal perto de 4.5:1 ou melhor e componentes/bordas funcionais perto de 3:1 ou melhor. Informação crítica nunca pode depender só de vermelho/verde: combina cor com forma, ícone, padrão ou texto.

### 4. Modelos das nove torres

Mantém desenho procedural em Gloss e dá a cada torre uma silhueta reconhecível mesmo em cinzento e a 70% do tamanho normal:

- **Sentinela**: base quadrada compacta, ameias e um único núcleo frontal; leitura de torre básica fiável.
- **Glaciar**: cristal vertical facetado, pontas assimétricas e halo frio; nunca parecer apenas uma Sentinela azul.
- **Braseiro**: taça/fornalha larga com chama central e grelhas laterais.
- **Pânico**: orbe suspenso dentro de uma armação curva, com movimento instável e olho/runa central.
- **Venenoide**: reservatório/frasco com tubos, gota ou válvula visível.
- **Tesla**: duas bobinas e arco elétrico curto; silhueta aberta no topo.
- **Impacto**: canhão pesado, baixo e orientado; comunicar peso e recuo.
- **Solar**: disco ou pétalas refletoras em torno de um núcleo; aparência de suporte ofensivo.
- **Tempestade**: torre assimétrica de endgame, com três condutores, anel de energia e presença maior sem tapar o caminho.

Cria progressão visual por patamares, não adicionando apenas pequenos retângulos:

- nível inicial: silhueta e núcleo;
- nível intermédio: estrutura mais larga, segundo material e energia mais estável;
- nível alto: mudança clara de topo/base e aura controlada;
- especialização `DANO+`: placas maiores, núcleo único mais intenso, cano/emissor pesado;
- especialização `RÁPIDA`: emissores duplicados, peças leves, bobinas/aneis que sugerem cadência.

Cada upgrade deve ser reconhecível durante o jogo sem abrir o painel. Limita o custo: define um orçamento aproximado de primitivas por torre e reutiliza submodelos puros. Evita detalhes menores do que dois ou três píxeis à escala mínima.

### 5. Inimigos e bosses

Preserva as classes atuais, mas reforça silhueta, massa e counter:

- rápido: corpo inclinado e seta/cauda direcional;
- tanque: largo e pesado;
- blindado: placas e contorno duplo;
- regenerador: símbolo de pulso/folha, não apenas verde;
- dispersor: forma radial ou segmentada;
- protegido: escudo externo claramente separado da barra de vida;
- elite: coroa ou chevron e escala ligeiramente superior;
- bosses: formas únicas, barra especial e telegráficos próprios.

Para bosses:

- cria uma barra de boss separada do HUD normal;
- mostra nome, fase e habilidade ativa;
- faz o Ariete antecipar a aceleração com compressão/clarão curto;
- faz o Bastião comunicar quais aliados estão protegidos através de linhas ou pulsos discretos;
- faz o Nexo da Ruptura marcar a zona perigosa com limite, preenchimento muito leve e pulso de ativação;
- garante que auras continuam legíveis sobre relva, terra, asfalto e água;
- inclui opção de reduzir pulsos/efeitos.

### 6. Mapa, portal e base

Melhora o mapa sem esconder a grelha lógica:

- cria pequenas variações determinísticas de relva, sem ruído aleatório por frame;
- adiciona borda visual simples entre caminho e relva;
- dá direção visual ao asfalto sem o confundir com água;
- anima água apenas com duas ou três linhas/tons lentos;
- usa sombras suaves e consistentes para torres, inimigos, base e portal;
- torna células válidas de construção visíveis apenas durante colocação;
- mantém o preview verde/vermelho com ícone de confirmação/proibição, não apenas cor.

Redesenha proceduralmente:

- **portal**: aro rúnico compacto, centro animado e indicação da próxima saída;
- **base**: fortaleza pequena e imediatamente identificável, com estado visual saudável/danificado/crítico;
- quando a base sofre dano, usa flash curto e indicador direcional; screen shake deve ser subtil, opcional e desativável.

### 7. UI e UX

Mantém a estrutura funcional atual — HUD superior, arsenal à esquerda e painel à direita — mas melhora densidade e hierarquia.

#### HUD

- reduz molduras decorativas e dá prioridade a vida, créditos, vaga e velocidade;
- acrescenta barra de progresso da vaga baseada em dados reais de `UIState`;
- cria aviso de perigo quando inimigos entram numa zona próxima da base;
- mostra mutadores com ícone + sigla + tooltip;
- não repitas `VEL` e os botões 1x/2x/4x de forma visualmente pesada.

#### Arsenal

- cria ícones procedurais consistentes para as nove torres;
- mantém nome curto, preço, estado bloqueado/comprável/selecionado;
- dinheiro insuficiente deve usar opacidade, ícone e texto, não só vermelho;
- adiciona tooltip com papel, alvo prioritário, efeito e dois stats principais;
- implementa scroll/categorias apenas se houver overflow real; com nove torres, prefere resolver primeiro legibilidade e espaçamento.

#### Painel contextual

- separa mapa/vaga, torre e ações através de títulos e divisores claros;
- usa alinhamento em grelha e números tabulares visualmente estáveis;
- substitui siglas ambíguas por ícones acompanhados de texto;
- mostra comparação de upgrade em duas colunas ou linhas `atual → novo`;
- torna `DANO+` e `RÁPIDA` visualmente simétricos, com ícones próprios e consequências resumidas;
- mantém todas as hitboxes derivadas dos mesmos `UIRect` usados no desenho.

#### Tipografia

- reconhece que `UIText` usa texto vetorial do Gloss, não bitmap;
- evita converter automaticamente frases longas para maiúsculas;
- define estilos `Title`, `Section`, `Body`, `Caption`, `Number` e `Button`;
- centraliza escalas, altura de linha e cores;
- melhora centering/medição: não assumes que todos os caracteres têm a mesma largura quando isso causar desalinhamento;
- usa maiúsculas em títulos e botões curtos, não em parágrafos ou tooltips;
- testa palavras portuguesas longas e números grandes.

### 8. Ícones e linguagem visual

Cria uma biblioteca pequena de ícones procedurais, por exemplo `UIIcons.hs`, sem depender de emojis ou glyphs do sistema:

- vida/base;
- créditos;
- vaga e inimigo;
- velocidade e pausa;
- dano, alcance, rajada e cadência;
- alvo prioritário;
- armadura, escudo, regeneração e resistência a área;
- fogo, gelo, resina, medo, veneno e elétrico;
- aviso, erro, bloqueado e informação.

Cada ícone deve funcionar a uma cor, ter silhueta distinta e continuar legível entre 14 e 24 unidades visuais. Usa texto junto do ícone na primeira exposição ao jogador.

### 9. Efeitos e animação

Implementa animações curtas baseadas no tempo já existente, sem criar estado complexo quando uma função pura for suficiente:

- hover/foco: 80–150 ms;
- confirmação de clique/compra: 120–200 ms;
- disparo e impacto: 100–250 ms;
- upgrade: 350–650 ms;
- morte normal: 180–300 ms;
- entrada/fase de boss: 600–1200 ms com aviso antecipado.

Melhora:

- projéteis com origem e destino legíveis;
- impacto diferente por elemento;
- dano flutuante agregado para evitar spam;
- morte com escala/fade simples;
- upgrade com anel, brilho e troca de silhueta;
- seleção com contorno e alcance limpo;
- preview inválido com cruz/padrão.

Adiciona uma opção `Efeitos: Completo / Reduzido`. O modo reduzido deve remover pulsos fortes, screen shake e excesso de partículas, mantendo toda a informação funcional.

### 10. Assets atuais

Audita os BMP carregados. Não mistures automaticamente a ilustração cartoon do menu, o inimigo clipart, a torre 3D e o portal detalhado na mesma cena.

Escolhe uma política explícita:

- preferencialmente usa desenho procedural coeso durante gameplay;
- mantém a imagem de fundo apenas como splash/menu se combinar com a nova UI e se não prejudicar texto;
- remove carregamento e tipos de imagem que não são realmente usados, ou documenta por que ficam preparados;
- não cries novos assets copiados de jogos comerciais;
- se forem necessários novos bitmaps, define primeiro guia de tamanho, perspetiva, contorno, luz e fundo transparente.

### 11. Organização técnica

Reduz o risco de continuar a aumentar `Desenhar.hs`. Extrai gradualmente, sem uma reescrita total, módulos como:

- `VisualTheme.hs`;
- `UIIcons.hs`;
- `TowerVisuals.hs`;
- `EnemyVisuals.hs`;
- `EffectsVisuals.hs`;
- opcionalmente `HUDVisuals.hs` e `MapVisuals.hs`.

Mantém funções puras que devolvem `Picture`, evita dependências circulares, atualiza `immutable-towers.cabal` e conserva uma ordem de camadas explícita. Não dupliques geometria entre render e input.

### 12. Fases de implementação

Executa por esta ordem:

1. baseline, screenshots e inventário visual;
2. `VisualTheme` e contraste;
3. tipografia, botões, painéis e ícones;
4. HUD, barra de vaga e perigo da base;
5. modelos e upgrades das torres;
6. classes de inimigos e telegráficos de bosses;
7. mapa, portal, base, projéteis e impactos;
8. opções de efeitos/high contrast;
9. remoção de assets/código visual morto;
10. playtest multirresolução e documentação.

Depois de cada fase:

- executa build e testes;
- mostra os ficheiros alterados e a decisão visual;
- verifica que não existem elementos fora do ecrã;
- compara antes/depois com imagem quando possível;
- corrige regressões antes de avançar.

### 13. Critérios de aceitação

O trabalho só está concluído quando:

- as nove torres são distinguíveis por silhueta sem depender da cor;
- classes especiais e bosses são reconhecíveis à escala normal de jogo;
- base, portal e caminho continuam claros numa vaga cheia;
- UI crítica não depende apenas de vermelho/verde;
- texto principal e secundário permanecem legíveis em todos os painéis;
- a barra de vaga e o aviso de perigo usam dados reais;
- efeitos reduzidos mantêm toda a informação de combate;
- hitboxes continuam alinhadas com o desenho;
- 1280x720, 1600x900, 1920x1080 e 2560x1440 passam a checklist visual;
- `cabal build` e `cabal test` passam sem regressões;
- o vault é atualizado com estado, changelog, backlog e decisões visuais;
- não ficam assets carregados sem uso ou estilos visuais incompatíveis sem justificação.

No final entrega um resumo curto, screenshots antes/depois, resultados dos testes, checklist manual, limitações ainda abertas e os três próximos passos de maior impacto. Começa pela inspeção e pelo baseline; depois implementa, não fiques apenas pelo plano.

## Referências de princípio

Usar apenas como estudo de clareza, nunca para copiar arte ou identidade:

- Kingdom Rush: especializações e leitura rápida de torres — https://play.kingdomrush.com/kingdom-rush-presskit
- Plants vs. Zombies Replanted: high contrast, redução de screen shake e controlo de velocidade — https://www.ea.com/able/resources/plants-vs-zombies-replanted
- Microsoft, acessibilidade em jogos: forma/padrão além da cor — https://learn.microsoft.com/en-us/windows/uwp/gaming/accessibility-for-games
- Apple HIG, acessibilidade e contraste — https://developer.apple.com/design/human-interface-guidelines/accessibility/
