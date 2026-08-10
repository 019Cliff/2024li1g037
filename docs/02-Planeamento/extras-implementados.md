# Immutable Towers - Extras Implementados

Tags: #estado #extras

Este documento resume as funcionalidades extra acrescentadas ao Immutable Towers.

Para o estado jogavel atual e correcoes recentes, ver [[estado-atual]].

## Novos projeteis

- Fogo: dano direto e dano continuo.
- Gelo: controlo forte, parando o inimigo temporariamente.
- Resina: abranda inimigos.
- Medo: faz o inimigo recuar durante a duracao do efeito.
- Veneno: causa dano continuo mais agressivo.
- Eletrico: atordoa por pouco tempo e funciona bem contra grupos por ter rajada maior.

## Sinergias

- Fogo + Resina prolonga o fogo.
- Gelo + Eletrico cria atordoamento eletrico adicional.
- Medo + Veneno aumenta o tempo de panico.
- Fogo + Eletrico causa dano extra imediato.
- Veneno + Resina causa dano extra.

## Terrenos especiais

- Asfalto: terreno caminhavel que aumenta a velocidade dos inimigos.
- O mapa avancado troca o caminho principal por asfalto, criando uma variante mais dificil.

## Inimigos com comportamento variado

- basico, rapido, tanque, blindado, regenerador, dispersor, protegido e elite
- tres bosses com fases/auras diferentes
- armadura, resistencia direta/area, escudo e regeneracao
- ondas escalam vida, velocidade, dano e butim

## Melhorias de torres

O jogador pode selecionar uma torre colocada e carregar em `U` para gastar creditos. O upgrade melhora:

- dano
- alcance
- velocidade de disparo
- rajada, em torres mais fortes
- duracao do efeito do projetil

O preco do upgrade tambem escala com o poder atual da torre.

Melhorias recentes:

- preview de upgrade no painel lateral antes da compra
- feedback visual no momento do upgrade
- modelos das torres com mais detalhes visuais conforme o poder/upgrade
- prioridades de alvo reais por identidade
- especializacao permanente entre dano/alcance e rajada/cadencia
- niveis maximos configurados por torre

## Modos de jogo

- Historia: ondas equilibradas.
- Infinito: sobrevivencia continua com mutadores Fortificados, Butim Reduzido e Onda Dupla.
- Desafio: menos vida e ondas mais fortes.
- Boss: poucas ondas, mas inimigos duros.
- Sandbox: muitos creditos para testar torres e upgrades.

O Godot agrega cada bloco 3x3 do mapa-fonte numa célula global grande. A grelha jogável passa de 36x34 para 12x11 e cada torre ocupa apenas uma dessas células, tal como estrada, água, portal, base, inimigos e obstáculos. Cada modo tem também um limite: 18 História, 24 Infinito, 12 Desafio, 16 Bosses e 30 Sandbox. Vender liberta capacidade e runs antigos são migrados sem perder torres.

## Guardar e carregar

- `S`: guarda o jogo atual em `immutable-towers-save.txt`.
- `L`: carrega o ultimo jogo guardado.

## Progressao

- Perfil local com jogos, vitorias, derrotas e melhor pontuacao.
- Desbloqueio progressivo de torres.
- Gemas e baus.
- Fusao para a torre Tempestade.

## Editor de mapa

- `E` no menu abre o editor.
- Clicar numa celula alterna entre relva, terra, asfalto e agua.
- Edicoes que bloqueiem o caminho ou invalidem base, portal ou torres sao rejeitadas.
- `ENTER` ou `ESC` volta ao menu.

## Obstaculos dinamicos

- `O` durante o jogo transforma a celula de caminho sob o rato em relva, funcionando como obstaculo simples.

## Bot / sugestao

- Haskell e Godot possuem sugestão contextual e bot automático opcional.
- O Godot compara construção, upgrade e especialização pela mesma API do jogador.
- Poupar é uma decisão válida, exige rendimento provável e uma defesa mínima já preparada.
- O planeador usa memória curta e um rollout reduzido sobre no máximo 12 ações.
- A estratégia atual é determinística, mas ainda precisa de melhor posicionamento e diversidade; a evolução está no [[plano-melhoria-total-2026-07-29#Fase 3 - Bot 3.0|plano do Bot 3.0]].

## Integridade e balanceamento

- Compra, upgrades e venda usam o investimento realmente pago; refund fixo de 65%.
- Elites e bosses têm tenacidade e diminishing returns contra controlo repetido.
- Gelo mantém movimento mínimo, eletricidade tem stun curto e medo não faz bosses recuar.
- A pontuação não recompensa espera nem torres sem utilidade.
- Os cinco estágios de História têm perfis próprios, assinaturas por mapa e escalamento por capítulo.
- O Infinito recompensa a vaga alcançada com uma curva crescente de gemas, multiplicador 2,5x, retornos decrescentes e limite de 188 por partida.
- A melhor vaga do Infinito é persistida por conta e a recompensa aparece no ecrã final.
- O runner automático inclui uma matriz económica e nove partidas finitas anti-stalemate.

## Interface e grafismo

- O mapa Godot usa células globais de 66 px, equivalentes a 3x3 células antigas de 22 px, e ganhou contador de capacidade.
- A shop passou a funcionar numa sidebar lateral esquerda.
- A loja meta Godot ganhou três modelos procedurais de baú, abertura e revelação animadas.
- O fundo do menu Godot representa uma batalha procedural com estrada, portal, inimigos, torres e projéteis.
- Novos efeitos visuais indicam fogo, gelo, resina, medo, veneno e eletricidade.
- Os disparos agora mostram um impacto visual mais claro no alvo.
- A UI passou a bloquear corretamente o clique no mapa por baixo.
- O editor e os atalhos tornam as funcionalidades extra acessiveis sem menus complexos.
