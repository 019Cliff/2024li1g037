# Immutable Towers - Baseline Protegido da Migração Godot

Tags: #migracao #baseline #arquivo

Relacionadas: [[Indice-Migracao-Godot]], [[diario-migracao]], [[00-Inicio/estado-atual|Estado Atual]]

Data: 2026-07-13

> Nota histórica: esta nota congela o ponto de partida de 2026-07-13. As limitações no fim eram verdadeiras nesse dia e não representam o estado atual. Consultar [[00-Inicio/estado-atual|Estado Atual]] e [[03-Qualidade-e-Release/auditoria-2026-07-28|Auditoria 2026-07-28]].

## Git

- Branch: `main`
- HEAD: `069e82c85e62a65c8c553fc54d5fee32d6fd8996`
- Remote: `origin` -> `https://github.com/tomasd005/2024li1g037.git`
- Worktree: sujo antes da migracao, com alteracoes Haskell, testes e vault ainda nao consolidadas.
- Nao existe commit/snapshot limpo criado para esta migracao.

Por seguranca, a migracao adicionou apenas `godot/` e a documentacao hoje organizada em `docs/04-Migracao-Godot/`. Nenhuma alteracao preexistente foi revertida, staged ou commitada nessa fase.

## Haskell

- `cabal build all`: passou.
- `cabal test --test-show-details=direct`: 166/166 testes passaram, zero erros e zero falhas.
- O projeto Haskell continua a ser a versao principal e o launcher existente nao foi alterado.

## Godot

- `godot`/`godot4` nao estavam no `PATH`; foi autorizada uma copia portatil fora do repositorio.
- Executavel: `%USERPROFILE%/.codex/tools/godot-4.7/bin/Godot_v4.7-stable_win64.exe`.
- Versao: `4.7.stable.official.5b4e0cb0f`, publicada em 2026-06-18.
- SHA-256 do ZIP oficial: `02A5312236F4E0209C78BCB2F52135B1963E6B8888C873C9CEE81459E60BCD71`.
- Renderer alvo: Compatibility.
- Importacao headless: passou.
- Smoke test da cena principal: passou.
- Testes headless iniciais: `PASS (6 checks)`.

## Sistemas encontrados

- cinco mapas e cinco modos;
- nove torres com identidade runtime, niveis e especializacao;
- oito classes normais e tres bosses;
- efeitos, armadura, resistencias, ondas, mutadores e progressao;
- bot estrategico parcial e deterministico;
- loja meta transacional parcial;
- tipos/armazenamento de contas locais ainda nao ligados ao fluxo principal;
- save global V2 com fallback legacy;
- UI Gloss em espaco virtual 1920x1080.

## Assets

Os BMP em `app/imagens/` nao apresentam metadados de licenca suficientes no repositorio. Nao sao copiados para Godot. O scaffold usa apenas cores, formas e placeholders procedurais.

## Limitacoes do baseline

- runtime/editor Godot disponivel apenas como copia portatil local;
- sem export templates Godot instalados;
- sem bundle Godot;
- sem fixture JSON de paridade ainda;
- worktree Haskell sujo, preservado intencionalmente.

Estas limitações foram superadas em grande parte durante 2026-07-13/15, exceto o worktree sem checkpoint, que continua aberto.
