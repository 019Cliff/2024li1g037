# Immutable Towers - Godot 4.7

Migracao lado a lado. O jogo Haskell/Gloss continua presente e executavel na raiz ate aprovacao explicita do cutover.

## Executar

Use `run-godot.bat`. O script encontra `GODOT_EXE`, `godot`/`godot4` no PATH ou a copia portatil de desenvolvimento em `%USERPROFILE%/.codex/tools/godot-4.7/bin`.

## Testes

Use `run-tests.bat`. O runner headless devolve o exit code real, executa 250 checks de catalogo Haskell, conversao da grelha global 3x, limites de torres, ondas, campanha, combate, controlo, economia, recompensas, pontuacao, bot, contas, loja, saves, editor e obstaculos, e termina com um gate de 45 transacoes e nove partidas finitas.

## Bundle Windows

Use `build-portable-windows.bat`. O resultado fica em `release/godot-windows-portable` e em `release/ImmutableTowers-Godot-Windows.zip`, com executavel, PCK, licenca Godot, README e `run-game.bat`. Este bundle nao substitui o bundle Haskell.

## Dados locais

Godot guarda contas em `%APPDATA%/Godot/app_userdata/Immutable Towers/accounts`. Cada conta possui o seu perfil, progresso, loja, ranking e partida pendente. Escritas importantes usam temporario e backup.

## Estado

Gameplay completo migrado: nove torres, onze inimigos, cinco mapas/modos, ondas, bosses, efeitos, obstaculos, bot, contas, loja, save/load e editor. Opcoes persistentes, almanaques, creditos e HUD/painel recolhiveis tambem estao ligados. Consulte `docs/04-Migracao-Godot/matriz-paridade.md` para os playtests manuais ainda abertos.
