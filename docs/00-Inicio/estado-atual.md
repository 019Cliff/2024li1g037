# Immutable Towers - Estado Atual

Tags: #estado #hub

Resumo vivo do estado jogável. Detalhes técnicos ficam nas notas de sistema, evidência datada fica na [[03-Qualidade-e-Release/auditoria-2026-07-29|última auditoria]] e trabalho aberto fica apenas em [[backlog-jogo]].

Relacionadas: [[HOME]], [[extras-implementados]], [[roadmap-atual]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]

## Ultima atualizacao

- Data: 2026-08-10
- Build: OK
- Testes Haskell: 166/166
- Testes Godot: 250/250 + gate de gameplay
- Vault: 39 notas organizadas, sem links partidos, nomes duplicados ou notas sem tags

## Migracao Godot

- Godot 4.7 funciona lado a lado e já é o candidato principal de evolução
- o launcher da raiz continua a abrir Haskell até aprovação explícita do cutover
- gameplay, contas locais, loja, bot, saves, importação Haskell/Godot e editor estão jogáveis
- o ZIP Windows atualizado foi confirmado com 84 105 093 bytes e o hash registado na documentação de distribuição
- o ZIP arranca isoladamente, mas usa o executável completo do editor e apresenta `(DEBUG)`; é bundle de desenvolvimento, não release final
- a matriz viva e os gates restantes estão em [[04-Migracao-Godot/matriz-paridade]]
- decisão atual de cutover: **ADIAR**

## Bloqueadores atuais

1. o bot com arsenal completo ainda não provou superar Sentinela-only nos cinco mapas;
2. Desafio e Bosses resolvem sem stalemate, mas ainda precisam de uma baseline de vitória/equilíbrio aprovada;
3. o pacote Windows não é um export release real;
4. a migração ainda não tem checkpoint Git e falta o segundo computador físico;
5. a UI precisa de reflow em 4:3/21:9 e validação de escala Windows.

Diagnóstico e evidência: [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]].

## Estado geral

O candidato Godot está jogável com menu, perfis e rankings locais, cinco modos, cinco mapas rotativos, campanha, loja, upgrades, classes de inimigos, efeitos, save/load, editor de mapa e bot automático opcional. O Haskell permanece funcional como referência e rollback.

## Torres

- nove identidades configuradas numa unica fonte `TowerSpec`
- `TowerId`, nivel e especializacao guardados explicitamente por torre construida
- prioridades reais: primeiro na rota, mais rapido, mais vida e maior grupo
- nivel maximo diferente por torre
- escolha permanente entre especializacao de dano/alcance e rajada/cadencia
- painel com DPS aproximado, stats, prioridade, custo, preview e refund
- modelos, cores e evolucao visual ligados a identidade runtime
- limite estratégico por modo: História 18, Infinito 24, Desafio 12, Bosses 16 e Livre 30
- o Godot agrega cada bloco 3x3 do mapa-fonte numa célula global; cada torre ocupa uma dessas células grandes, tal como estrada, água, portal, base e obstáculos
- vender uma torre liberta capacidade; saves antigos acima do limite mantêm todas as torres, mas não permitem novas construções
- a escala foi aprovada: os mapas têm 90-107 células de relva, mas só 30-47 posições ficam no alcance útil de uma Sentinela N1 e História limita a 18 torres
- compra e upgrades guardam o investimento realmente pago; a venda devolve 65% desse investimento e nunca gera lucro
- saves antigos recebem uma estimativa conservadora do investimento, sem apagar torres
- a matriz automática cobre nove torres, cinco modos, níveis e especializações

Ver [[sistema-torres]].

## Inimigos e combate

- classes: basico, rapido, tanque, blindado, regenerador, dispersor, protegido e elite
- bosses: Ariete Veloz, Bastiao Vivo e Nexo da Ruptura
- armadura, resistencia direta/area, escudo, regeneracao e fases simples
- Guardiao protege aliados proximos; Ruptura enfraquece torres dentro da aura
- velocidade base separada da velocidade efetiva, evitando inimigos presos apos gelo/resina
- chegada a base detetada mesmo quando um inimigo atravessa a posicao da base entre frames
- spatial grid mantem a procura de alvos escalavel
- impactos, estados, barras de vida e numeros de dano visiveis
- gelo abranda com velocidade mínima, eletricidade atordoa por períodos curtos e medo em bosses transforma-se num abrandamento para a frente
- caps, tenacidade por classe e diminishing returns impedem acumulação ilimitada de controlo
- exposição e janela de controlo sobrevivem a save/load

Ver [[sistema-inimigos]].

## Ondas, modos e mapas

- composicoes declarativas por grupos de classes
- historia com dez vagas por estagio e cinco estagios distintos por capitulo
- `RunDifficultyProfile` combina capítulo, estágio e mapa; cada mapa introduz uma classe assinatura e os estágios escalam composição/atributos
- infinito gera vagas continuamente e aplica Fortificados, Butim Reduzido e Onda Dupla em marcos fixos
- Infinito paga gemas pela vaga alcançada, com multiplicador 2,5x, retornos decrescentes, limite de 188 e melhor vaga persistida por conta
- desafio, boss e sandbox usam composicoes e economias proprias
- preview textual da proxima vaga no painel lateral
- editor rejeita alteracoes que removam o caminho portal-base, movam o piso da base/portal ou invalidem torres
- cinco mapas rotativos com terreno normal, asfalto e agua
- o catálogo Haskell preserva os mapas-fonte 36x34; o runtime Godot converte-os para uma grelha jogável 12x11 com células de 66 px
- distâncias de movimento, alcance e auras são convertidas por `1/3`, evitando triplicar o alcance efetivo quando a célula aumenta
- runs e mapas personalizados antigos são migrados automaticamente para a célula global correspondente
- o estágio 1 preserva a tabela de referência; estágios 2-5 acrescentam assinaturas, substituições, suporte, elite final e escalamento progressivo
- Desafio e Bosses terminam de forma consistente no gate automático; a baseline atual ainda resulta em derrota e precisa de balanceamento

Ver [[sistema-ondas]].

## UI e input

- espaco virtual 1920x1080 com escala para varias resolucoes
- HUD superior, loja lateral esquerda e painel contextual direito
- painel direito usa a mesma `gamePanelRect` para desenho e bloqueio de input
- cliques sobre UI nao constroem torres no mapa
- troca de torre selecionada nao dispara construcao acidental
- pausa, velocidades 1x/2x/4x, HUD recolhivel e loja recolhivel
- ecras dedicados de vitoria e derrota
- submenus com fundo animado consistente e botoes `Voltar` clicaveis
- fundo do menu Godot com paisagem em camadas, estrada sinuosa, portal, inimigos, torres, projéteis e partículas; `reduced_effects` reduz o movimento
- rodapé do mapa mostra nome e capacidade `torres usadas / limite`
- opcoes funcionais e persistentes: ecra inteiro, reducao de efeitos e numeros de dano
- almanaques rolaveis de torres e inimigos gerados pelo catalogo comum
- ecrã de creditos e atalhos H/K/A/B/O para HUD, painel, bot, sugestao e obstaculo
- obstaculos dinamicos abrandam inimigos sem invalidar a rota portal-base
- tema visual central em `VisualTheme.hs` e icones Gloss reutilizaveis no HUD
- o Godot é utilizável a 1280x720 e 1920x1080, mas a 1024x768 texto/modelos ficam pequenos
- em 21:9 a partida fica ancorada à esquerda; em 4:3 fica no topo; a cena ainda depende de offsets fixos
- o fundo do menu é rico, mas demasiado escuro/coberto; os baús precisam de mais escala e mensagens sem sobreposição

Ver [[sistema-ui]].

## Progressao e persistencia

- no Godot, perfil, ranking, gemas, baús, desbloqueios, fusão e partida pendente pertencem à conta ativa
- primeira execução permite criar ou importar uma conta; criar, selecionar, renomear, terminar sessão, exportar e eliminar estão ligados à UI
- o jogador pode escolher se a última conta fica lembrada e a UI explica que não existe palavra-passe nem cloud
- checkpoints de jogador, bot, vaga, pausa, perda de foco, fecho e regresso ao menu usam armazenamento versionado com `.tmp` e `.bak`
- saves Haskell podem ser exportados para JSON e importados sem alterar a origem
- contas Godot fazem round-trip pelo envelope `immutable-towers-account-export`
- colisões permitem cancelar, importar como nova ou substituir explicitamente
- o Haskell continua a arrancar pelo perfil global; a fundação de contas Haskell não foi ligada ao fluxo principal
- as contas atuais são locais, sem palavra-passe, backend ou sincronização cloud
- a loja Godot usa três modelos procedurais de baú, estados de saldo/recompensa, animação de abertura, revelação nova/duplicada, compensação e painel visual de fusão
- abrir e revelar são apresentação: a transação e a recompensa pendente ficam guardadas antes da animação e sobrevivem a uma interrupção
- fechar/reabrir o bundle recuperou a conta e a partida pendente neste computador; continua a faltar o teste físico noutro computador
- estatísticas do perfil são formatadas como inteiros
- a pontuação usa vagas, eliminações, vida da base, bónus de tempo decrescente, eficiência limitada e multiplicador do modo
- esperar ou construir torres sem utilidade nunca aumenta a pontuação

Ver [[sistema-progressao]], [[sistema-contas]] e [[sistema-loja]].

## Bot

- bot automático opcional e sugestão contextual estão funcionais no Haskell e no Godot
- a versão Godot compara construções, posições, upgrades e especializações e usa a mesma API económica do jogador
- a decisão é determinística; as cinco referências Haskell continuam legíveis, mas posição/score divergem intencionalmente porque usam a grelha-fonte 36x34 e o Godot decide na grelha global 12x11
- numa conta nova só a Sentinela está desbloqueada; nesse caso a repetição é uma restrição real do arsenal
- com arsenal avançado, o Bot 2.0 avalia armadura, resistências, escudo, regeneração, velocidade, grupos, controlo, cobertura marginal, redundância e eficiência económica
- upgrades são comparados pelo ganho marginal; a construção perde prioridade à medida que a capacidade fica preenchida
- poupar é uma decisão válida e explicável, limitada a objetivos atingíveis com rendimento provável e uma defesa mínima já preparada
- um rollout reduzido avalia no máximo 12 ações por decisão, prevendo dano, pressão, rendimento e reserva
- o planeador guarda objetivo, última ação e as cinco identidades recentes; o início reserva créditos para três defesas acessíveis
- a cadência automática usa tempo real e não acelera a 2x/4x
- cada ação mutável é guardada imediatamente; a sugestão apresenta razão, componentes do score e alternativas
- campanhas prolongadas mostram que o arsenal completo usa sobretudo Sentinela, Glaciar e Solar e perde para Sentinela-only nos cinco mapas
- partidas finitas do gate já terminam sem bloqueio, incluindo Cruzamento Solar e estratégias de controlo
- a comparação nos cinco mapas mantém 80 de vida em quatro mapas e é mais rápida em Lago Fraturado e Bastião Espiral, mas ainda perde 29,9 de vida no Cruzamento Solar; a gate de superioridade continua aberta

Ver [[sistema-bot]].

## Qualidade confirmada

- `cabal build`: OK
- `cabal test`: 166 casos, 0 erros, 0 falhas
- Godot headless: 250 checks, 0 erros
- o gate integrado valida 45 combinações económicas e nove partidas finitas sem arbitragem, erro de ação ou ausência prolongada de progresso
- os cinco estágios do capítulo 1 vencem automaticamente; vida final: 80, 80, 48,6, 47,8 e 80
- testes cobrem identidade, targeting, especializações, conversão 36x34 -> 12x11, célula global, limites de torres, migração de runs/mapas antigos, saves, resistências, bosses, mutadores, controlo, score, editor, loja visual, domínio de contas e bot
- instancias de igualdade e testes academicos foram limpos sem warnings orfaos ou funcoes parciais
- partida Godot validada por captura real em 1280x720, 1366x768, 1280x800, 1600x900, 1920x1080, 2560x1440 e 2560x1080
- barra superior Godot revalidada a 1280x720 com HUD e painel recolhiveis, sem sobreposicoes
- ZIP Godot extraido numa pasta temporaria isolada; executavel arrancou sem Haskell, Cabal ou instalacao de Godot, mas ainda é build `(DEBUG)`
- benchmark sintético de 600 inimigos/120 torres revalidado em três execuções: média 4,70-4,87 ms, p95 4,67-5,39 ms e máximo 36,60-45,77 ms
- vault com 0 wikilinks partidos e 0 nomes de notas duplicados
- round-trip nativo validado num repositório isolado: exportar, detetar colisão e importar como nova
- o novo harness percorre mapas, arsenais, campanhas, economia e partidas prolongadas e expôs problemas que os checks unitários não detetavam
- menu, loja e partida foram revistos numa aplicação real e capturados em 1024x768, 1920x1080 e 2560x1080

## Próxima ação

Seguir [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]: afinar posicionamento/diversidade do Bot 3.0 até superar Sentinela-only, equilibrar Desafio/Bosses e avançar para o layout responsivo. Em paralelo, preparar export release. Não alterar o launcher principal antes da aprovação.
