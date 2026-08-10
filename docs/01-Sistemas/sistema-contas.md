# Immutable Towers - Contas locais

Tags: #sistema #contas #persistencia

Relacionadas: [[estado-atual]], [[sistema-progressao]], [[sistema-loja]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria]]

## Definição do produto atual

Uma “conta” é um perfil **local neste computador**. Não existe palavra-passe, PIN, servidor, recuperação por email ou sincronização cloud. O nome não deve ser apresentado como autenticação segura.

O backup JSON permite transportar uma conta entre computadores. Continua a não existir sincronização automática: um login online seria um sistema separado com backend, segurança e política de conflitos.

## Godot - implementação ativa

Módulos:

- `godot/src/autoload/app_state.gd`;
- `godot/src/persistence/account_repository.gd`;
- `godot/src/persistence/account_import_service.gd`;
- `godot/src/persistence/atomic_json_store.gd`;
- `godot/src/persistence/transfer_import_planner.gd`;
- `godot/src/persistence/legacy_run_converter.gd`;
- `godot/src/presentation/boot_screen.gd`.

Funcional:

- criar, selecionar, renomear e eliminar conta;
- nomes únicos sem diferenciar maiúsculas/minúsculas;
- `AccountId` separado do nome;
- última conta selecionada;
- perfil, progresso, gemas, coleção, loja, ranking, opções, recompensa e partida pendentes por conta;
- escritas por `.tmp` e `.bak`;
- importação idempotente do envelope Haskell;
- exportação e importação do envelope nativo Godot;
- política explícita de colisão: cancelar, importar como nova ou substituir;
- IDs aleatórios robustos gerados por `Crypto`;
- onboarding quando não existe conta;
- troca/terminação de sessão e opção para lembrar a última conta;
- arranque sem conta automática quando o armazenamento está vazio.

## Haskell - referência/rollback

`AccountTypes.hs`, `AccountSystem.hs` e `AccountStorage.hs` implementam tipos, validação e armazenamento versionado. O launcher Haskell ainda usa o perfil global e não liga esta camada ao fluxo principal.

## Estado de validação

- round-trip completo validado em armazenamento isolado;
- perfil, progresso, leaderboard, loja, recompensa, partida e opções são normalizados e preservados;
- colisão nunca substitui uma conta silenciosamente;
- recuperação de conta por `.bak` coberta por teste;
- primeira execução sem conta automática coberta por teste;
- falta apenas validar o transporte real para um segundo computador físico.

## Invariantes

- nome visível nunca é caminho de ficheiro;
- contas diferentes nunca partilham progresso;
- nenhuma importação altera o ficheiro de origem;
- nenhuma conta existente é substituída sem confirmação;
- ficheiro inválido não substitui o último ficheiro válido;
- eliminar uma conta não elimina outra;
- exportar/importar conserva todos os campos normalizados.

## Próxima fase

1. playtest manual com duas contas depois de reiniciar;
2. restaurar o backup num segundo computador;
3. testar eliminação/exportação pela UI com dados reais;
4. considerar cloud apenas como projeto separado, nunca como promessa implícita.
