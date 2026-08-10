# Immutable Towers - Contrato de Transferência de Saves

Tags: #migracao #save #contas

Relacionadas: [[sistema-contas]], [[matriz-paridade]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria]]

Estado: envelopes v1 Haskell e Godot, importadores e conversão de partidas implementados e testados.

## Envelope

- `schema`: `immutable-towers-transfer`
- `version`: `1`
- `source.game`: `haskell-gloss`
- `source.exporter_version`: `1`
- `legacy_global`: save global opcional
- `account_index`: indice Haskell opcional
- `accounts`: contas exportadas
- `warnings`: limitacoes encontradas pelo exportador

O schema verificavel esta em `migration/schema/transfer-v1.schema.json`. IDs persistidos usam ASCII minusculo, por exemplo `sentinela`, `planicie_serena` e `boss_ruptura`.

## Garantias

- o exportador reutiliza os decoders Haskell; Godot nunca interpreta `Show/Read`
- os ficheiros Haskell originais nunca sao modificados ou apagados
- escrita JSON por `.tmp`, validacao, promocao e `.bak`
- importacao idempotente por fingerprint SHA-256
- progresso Godot mais recente exige confirmacao antes de substituicao
- contas sao armazenadas separadamente em `user://accounts/<account-id>/profile-v1.json`
- perfil, ranking, meta, loja, recompensa e partida pendente pertencem a conta ativa
- checkpoints de partida são guardados após ações do jogador/bot, vagas, pausa, perda de foco, fecho e regresso ao menu

## Conversao de partida

`LegacyRunConverter` transforma o DTO Haskell num snapshot `immutable-towers-run` normal. Preserva base, creditos, mapa, torres, runtime, nivel, especializacao, cooldown, inimigos ativos, efeitos e vagas pendentes. A classe de um inimigo antigo e inferida pela mesma regra Haskell: menor distancia a `base_speed`. Mapas nao oficiais passam a mapa personalizado validado.

O snapshot Godot mantém `version = 1` e acrescenta `world_grid_scale = 3`. O catálogo/Haskell usa coordenadas-fonte 36x34; `WorldGrid` remapeia posições para a grelha Godot 12x11. Um run antigo sem a marca é tratado como coordenadas-fonte: torres e obstáculos passam à célula global correspondente e inimigos são projetados sobre o ponto de rota mais próximo. A migração é conservadora e não elimina torres que convergem para a mesma célula ou ficam acima do limite. Mapas personalizados v1 grandes são agregados e voltam a ser guardados como v2.

Depois da importacao, guardar/carregar usa apenas o formato Godot. Snapshots Godot antigos com `pending_classes` continuam suportados.

## Validação

Fixtures sanitizadas e duas contas independentes passam nos testes. Em 2026-07-15, uma copia do save real foi exportada e importada pelo fluxo da UI para a conta `legacy-global`; o SHA-256 do ficheiro Haskell original permaneceu inalterado. O validador reutilizavel esta em `godot/tools/validate_transfer_import.gd`.

## Envelope nativo Godot

`AccountRepository.export_account` produz:

- `schema`: `immutable-towers-account-export`;
- `version`: `1`;
- `exported_account`: documento completo da conta.

`AccountImportService` reconhece o envelope nativo e o envelope Haskell antes de delegar a conversão. Para uma conta nativa:

1. valida schema e versão;
2. normaliza todos os campos;
3. deteta colisão por `AccountId`;
4. oferece cancelar, importar como nova ou substituir;
5. usa um ID robusto e nome único na cópia;
6. mantém origem, `.tmp`, `.bak` e rollback intactos.

O validador `godot/tools/validate_native_account_roundtrip.gd` executa exportação, importação limpa e colisão real pela UI num armazenamento isolado.
