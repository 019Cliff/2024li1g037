# Immutable Towers - Loja meta

Tags: #sistema #loja #economia

Relacionadas: [[estado-atual]], [[sistema-contas]], [[sistema-progressao]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]], [[02-Planeamento/plano-melhoria-total-2026-07-29#6.3 Loja de baús|Plano Visual]]

## Domínio

Existem duas implementações equivalentes:

- Haskell: `app/ShopSystem.hs`;
- Godot: `godot/src/economy/meta_shop.gd`.

Regras atuais:

- baú de Madeira: 35 gemas;
- baú de Cristal: 90 gemas;
- baú Imperial: 170 gemas;
- fusão Tempestade: Tesla + Solar + 180 gemas;
- compra valida saldo antes da transação;
- duplicado devolve uma compensação explícita;
- seed determinística baseada no progresso;
- recompensa pendente fica guardada na conta;
- coleção é derivada de `unlocked_tower_ids`.

## UI Godot atual

A loja continua coordenada por `boot_screen.gd`, mas já possui componentes próprios:

- `shop_chest_card.tscn/.gd` desenha modelos distintos de Madeira, Cristal e Imperial;
- cada card apresenta preço, pool, saldo insuficiente e bloqueio por recompensa pendente;
- hover, brilho, flutuação, abertura da tampa, feixe e partículas dão feedback sem assets externos;
- `shop_reward_reveal.tscn/.gd` distingue torre nova de duplicado e mostra a compensação em gemas;
- a fusão mostra Tesla, Solar, custo e requisitos em falta;
- a animação respeita `reduced_effects`;
- a transação é concluída e guardada antes da animação, pelo que fechar durante a abertura não volta a cobrar nem perde o prémio;
- uma recompensa pendente reabre diretamente no estado de revelação;
- comprar altera a coleção antes de “recolher”; recolher encerra a apresentação persistente, não aplica uma segunda transação.

Ficam abertos a grelha de coleção, detalhe por torre e a extração completa do controlador de loja para fora de `boot_screen.gd`.

### Resultado visual de 2026-07-29

- os três modelos existem e a transação é segura;
- a 1024x768 os baús perdem protagonismo e o texto fica pequeno;
- os cards têm demasiado espaço vazio em relação ao modelo;
- `GEMAS INSUFICIENTES` atravessa o baú;
- é necessário um pedestal/sombra, modelo maior e hierarquia preço -> estado -> ação;
- a mensagem de bloqueio deve ficar abaixo do modelo;
- 4:3, 16:9 e 21:9 precisam de layout próprio/centrado.

## Invariantes

- nenhuma compra deixa gemas negativas;
- a recompensa e calculada antes da animacao;
- duplicados nao mencionam ouro inexistente;
- a colecao e sempre derivada de `torresDesbloqueadas`;
- a logica de compra nao depende do renderer.

## Riscos a fechar antes do polimento

- sair durante a recompensa deve ser testado sem duplicar custo ou prémio;
- a seed deve ter teste de sequência longa e não apenas uma compra;
- decidir se é desejado um sistema de proteção/pity antes de o mostrar na UI.

## Próxima evolução

1. aumentar o baú e corrigir hierarquia/overlap responsivo;
2. criar grelha de coleção e painel de detalhe;
3. extrair a coordenação da loja de `boot_screen.gd` quando houver nova funcionalidade que o justifique;
4. adicionar teste manual de fechar a aplicação em cada momento da abertura;
5. permitir saltar a animação depois da primeira visualização e respeitar reduced effects;
6. decidir se é desejada confirmação antes da compra;
7. medir uma sequência longa da seed e decidir sobre pity antes de o mostrar;
8. manter todas as transações em `MetaShop`, fora do renderer.

Especificação completa: [[02-Planeamento/plano-melhoria-total-2026-07-29#6.3 Loja de baús|Loja de baús]].
