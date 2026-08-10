# Immutable Towers - Checklist de Regressao

Tags: #qa #checklist

Checklist rapida para validar o jogo antes de bundle, commit importante ou entrega.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[changelog-jogavel]]

## Preparação

- registar plataforma: Haskell ou Godot
- registar commit/branch e confirmar se existem alterações não versionadas
- executar `cabal build all`
- executar `cabal test --test-show-details=direct`
- executar `godot/run-tests.ps1`
- executar auditoria rápida de transações, stalemates e campanhas
- regenerar fixtures se o domínio Haskell mudou

## Contas Godot

- arrancar sem contas num armazenamento isolado
- criar conta A e conta B
- confirmar nomes únicos sem diferenciar maiúsculas/minúsculas
- trocar entre contas
- criar progresso diferente em cada uma
- fechar/reabrir e confirmar a última conta
- confirmar coleção, ranking, opções e partida independentes
- exportar conta A
- importar conta A num armazenamento limpo
- comparar progresso antes/depois
- testar colisão de ID sem overwrite silencioso
- eliminar B e confirmar que A permanece
- corromper o ficheiro principal e confirmar recuperação pelo `.bak`

## Menu

- abrir o jogo e confirmar que o menu principal aparece bem
- navegar entre botoes com rato e teclado
- abrir perfil, ranking, ajuda, opcoes e modos
- clicar em `Voltar` na Loja e nos restantes submenus
- confirmar que o texto principal esta legivel

## Perfil e progresso

- editar nome do perfil
- usar backspace
- confirmar que os dados persistem ao reabrir

## Inicio de partida

- entrar numa partida de historia
- entrar numa partida de infinito
- perder nas vagas 1, 10, 20 e 50 e confirmar 3, 30, 70 e 125 gemas
- confirmar que a recompensa aparece no ecrã final e persiste depois de reabrir
- confirmar que a melhor vaga do Infinito nunca diminui
- confirmar que nenhuma run atribui mais de 188 gemas
- confirmar que o mapa carrega e que a UI nao sai do ecran

## Loja e construcao

- abrir Loja e confirmar modelos diferentes para Madeira, Cristal e Imperial
- confirmar estados de saldo insuficiente e recompensa pendente sem depender apenas da cor
- abrir um baú e confirmar tampa, brilho e revelação; repetir com efeitos reduzidos
- confirmar que duplicado mostra compensação em gemas
- confirmar requisitos da fusão Tesla + Solar + 180 gemas
- selecionar uma torre na sidebar
- trocar para outra sem construir acidentalmente
- clicar em zonas de UI e confirmar que nao passa input para o mapa
- confirmar 12 colunas x 11 linhas e células grandes, sem microgrelha de construção 36x34
- confirmar que estrada, água, portal, base, inimigos e obstáculos usam a mesma escala das torres
- colocar torre em relva valida
- confirmar que o preview ocupa exatamente uma célula global
- colocar duas torres em células adjacentes e confirmar que ambas são legais e selecionáveis
- clicar na célula da torre e confirmar seleção
- tentar construir em estrada, água e célula ocupada e confirmar rejeição
- tentar colocar torre em zona invalida
- atingir o limite do modo e confirmar bloqueio com mensagem/contador
- vender uma torre no limite e confirmar que uma nova construção fica disponível
- carregar um save antigo 36x34/acima do limite e confirmar migração sem apagar torres

## Torre e upgrade

- selecionar torre colocada
- ver stats no painel lateral
- confirmar preview de upgrade
- chegar ao nivel de especializacao e testar `DANO+` e `RAPIDA`
- confirmar que o nivel maximo bloqueia novos upgrades
- fazer upgrade
- confirmar feedback visual do upgrade
- vender torre
- confirmar que compra/venda imediata nunca aumenta créditos em nenhum modo
- confirmar que compra, upgrades, especialização e venda devolvem no máximo a percentagem definida do investimento real
- guardar/carregar entre compra e venda e repetir a validação

## Combate

- confirmar que as vagas arrancam automaticamente
- confirmar que projeteis aparecem
- confirmar que inimigos perdem vida
- confirmar que aparecem numeros de dano
- confirmar que inimigos com gelo/resina nao ficam presos para sempre
- confirmar que controlo reaplicado respeita caps/diminishing returns
- confirmar que elites e bosses aplicam tenacidade
- confirmar que medo não recua bosses indefinidamente
- confirmar que uma vaga finita progride por dano, movimento ou mudança de fase
- confirmar que inimigos rapidos que atravessam a base entre frames causam dano e desaparecem
- confirmar que blindado/escudo recebem menos dano de forma percetivel
- confirmar a aura do Guardiao e a zona do boss de Ruptura
- confirmar que a proxima vaga aparece no painel

## Modos e velocidade

- trocar entre 1x, 2x e 4x
- pausar e retomar
- testar HUD on/off
- testar loja on/off
- terminar os cinco mapas de História no harness
- terminar Desafio e os três Bosses sem stalemate
- chegar aos marcos combinados do Infinito em 1x e 4x

## Bot

- comparar arsenal completo com Sentinela-only nos cinco mapas
- confirmar que o arsenal completo não tem resultado inferior
- confirmar construção vs upgrade antes de atingir a capacidade
- confirmar que `poupar` mostra objetivo e razão
- confirmar que o bot não poupa sem defesa mínima nem rendimento futuro
- confirmar resposta a rápido, blindado, regenerador, dispersor, protegido, elite e bosses
- confirmar diversidade mínima definida na baseline
- confirmar checkpoint imediato depois de cada ação mutável

## Save/Load

- guardar jogo
- carregar jogo
- confirmar que o estado principal volta corretamente
- confirmar que nivel, identidade e especializacao da torre sao preservados
- comprar um baú, sair durante a recompensa e retomar sem duplicar custo/prémio
- deixar o bot construir/upgrade, fechar antes da vaga seguinte e confirmar restore
- voltar ao menu durante uma vaga e continuar
- iniciar nova partida e confirmar descarte explícito do save anterior

## Editor

- tentar cortar o unico caminho entre portal e base
- confirmar que a alteracao e rejeitada com mensagem
- fazer uma alteracao segura e confirmar que e aplicada
- carregar um mapa personalizado antigo 36x34 e confirmar conversão para 12x11

## Build

- confirmar Haskell 166/166 ou explicar qualquer alteração intencional da contagem
- confirmar Godot 250/250 e gate de gameplay com nove partidas aprovado
- confirmar ausência de `SCRIPT ERROR`/`ERROR:` no log Godot
- medir benchmark estabilizado e pico frio separadamente
- confirmar que score não cresce por idle nem por construir torres inúteis

## Texto de produto

- procurar e remover textos visíveis `scaffold`
- procurar e remover textos visíveis `vertical slice`/`fatia vertical`
- procurar e remover “migração” do fluxo normal do jogador
- confirmar que “conta local” não promete cloud ou segurança

## Resoluções e input Godot

- 1024x768
- 1280x720
- 1366x768
- 1280x800
- 1600x900
- 1920x1080
- 2560x1440
- um ecrã 4:3
- um ecrã 21:9
- redimensionar durante menu e partida
- escala Windows 100%, 125% e 150% quando disponível
- navegar por rato e teclado
- observar o fundo do menu e confirmar movimento discreto sem prejudicar a leitura
- repetir menu e loja com `REDUZIR EFEITOS VISUAIS`

## Bundle Godot

- criar ZIP por export `--export-release`
- extrair fora do projeto
- arrancar sem Haskell/Cabal/Godot instalados
- confirmar título sem `(DEBUG)`
- confirmar que o executável não é o editor Godot
- confirmar que `tests/` e `tools/` não entram no pacote
- confirmar persistência após restart
- confirmar export/import de conta
- testar num segundo computador físico
- registar tamanho, SHA-256 e resultado no diário
