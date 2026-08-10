# Immutable Towers - Sistema Progressao

Tags: #sistema #progressao

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[extras-implementados]]
- [[prompt-mestre-loja-contas]]
- [[sistema-contas]]
- [[sistema-loja]]

## Responsabilidades

- perfil local
- ranking local
- gemas
- desbloqueios
- progressao por historia
- recompensas por vitoria

## Modulos principais

### Haskell

- `app/ImmutableTowers.hs`
- `app/ProgressionSystem.hs`
- `app/SaveSystem.hs`
- `app/ScoreSystem.hs`
- `app/GameFactory.hs`
- `app/AccountTypes.hs`
- `app/AccountSystem.hs`
- `app/AccountStorage.hs`
- `app/ShopSystem.hs`

### Godot

- `godot/src/autoload/app_state.gd`
- `godot/src/persistence/account_repository.gd`
- `godot/src/economy/meta_shop.gd`
- `godot/src/presentation/boot_screen.gd`
- `godot/src/presentation/vertical_slice_screen.gd`

## Estado Godot

- perfil e ranking por conta local;
- jogos, vitórias, derrotas e melhor pontuação;
- nível, gemas, torres desbloqueadas e fusões;
- capítulo, estágio e modos com nível mínimo;
- vitória atualiza perfil, ranking e progressão;
- o Infinito atribui gemas mesmo em derrota, de acordo com a vaga alcançada;
- a melhor vaga do Infinito fica guardada por conta;
- score determinístico calculado por vagas, eliminações, vida da base, tempo decrescente, eficiência limitada e multiplicador do modo;
- partida pendente e recompensa pendente por conta;
- opções por conta;
- criar, selecionar, renomear, exportar e eliminar ligados à UI;
- importação de saves Haskell.

## Estado Haskell

- nome do jogador
- jogos/vitorias/derrotas
- melhor pontuacao
- gemas
- torres desbloqueadas
- baus
- fusao para Tempestade
- capitulo/estagio
- dominio puro de compras, recompensas e compensacao de duplicados
- tipos versionados, indice e armazenamento recuperavel de contas locais
- cartoes de baus clicaveis na loja meta

O launcher Haskell ainda arranca pelo perfil global. A camada de contas Haskell continua como fundação não ligada ao fluxo principal.

## Recompensa do Infinito

`godot/src/gameplay/run_rewards.gd` concentra a regra para a UI e para a progressão usarem exatamente o mesmo valor.

| Vaga alcançada | Gemas |
|---:|---:|
| 1 | 3 |
| 5 | 13 |
| 10 | 30 |
| 15 | 50 |
| 20 | 70 |
| 30 | 95 |
| 50 | 125 |
| 100 | 173 |
| 120+ | 188 |

A curva base é multiplicada por 2,5, usa retornos decrescentes depois da vaga 15 e tem limite de 188 gemas por partida. Chegar à vaga 50 permite comprar um baú de Cristal; chegar à vaga 120 permite comprar um baú Imperial. Uma partida que ainda não alcançou a primeira vaga não recebe gemas. O ecrã final mostra a vaga alcançada e a recompensa antes de voltar ao menu.

## Lacunas atuais

- criar vagas/dificuldade próprias por capítulo, estágio, mapa e modo;
- concluir o playtest UI de criar, renomear, trocar, eliminar, exportar e importar;
- validar duas contas e backup num segundo computador físico;
- revelar desbloqueios com apresentação própria;
- validar a curva de nível/gemas por telemetria;
- criar grelha de coleção e detalhe por torre;
- separar eventual conta online num projeto de backend próprio.

O round-trip Godot, checkpoints do bot/fecho, onboarding, troca de conta e confirmação de nova partida já estão implementados; não devem voltar ao backlog salvo regressão.

Ver [[sistema-contas]], [[sistema-loja]], [[sistema-pontuacao]] e [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].
