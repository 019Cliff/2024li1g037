# Immutable Towers - Distribuição

Tags: #release #bundle

Relacionadas: [[checklist-regressao]], [[04-Migracao-Godot/checklist-cutover|Checklist de Cutover]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria]]

Existem duas distribuições em paralelo. Nenhuma deve substituir a outra até aprovação explícita do cutover.

## Godot Windows - candidato

### Criar

```powershell
.\godot\build-portable-windows.ps1
```

Resultado:

```text
release/
  ImmutableTowers-Godot-Windows.zip
  godot-windows-portable/
    ImmutableTowers.exe
    ImmutableTowers.pck
    LICENSE-GODOT.txt
    README.txt
    run-game.bat
```

O preset exclui `tests/*` e `tools/*`. Os dados do jogador não ficam no bundle; são guardados em:

```text
%APPDATA%\Godot\app_userdata\Immutable Towers\accounts
```

### Artefacto confirmado em 2026-07-30

- ZIP: 84 105 093 bytes;
- SHA-256: `4466E9604CFDEE31EA58713E63429570E84D54682C7F2694309B6D1AFE8C98DC`;
- extraído e iniciado fora do projeto sem Haskell, Cabal ou instalação de Godot;
- inclui contas nativas restauráveis, autosave completo, primeira fase do Bot 3.0, economia sem arbitragem, recompensa equilibrada do Infinito, controlo anti-stalemate, score corrigido, grelha global 12x11, migração de coordenadas antigas, limites de torres, fundo animado e loja com baús/revelação;
- carregou a conta e a partida pendente existentes;
- o título da janela é `Immutable Towers (DEBUG)`;
- o script exporta o PCK e copia o executável completo do editor Godot.
- ainda falta um segundo computador físico.

Este ZIP é um bundle portável de desenvolvimento. Não é o artefacto de produção final.

### Bloqueadores de distribuição

- instalar templates compatíveis e usar `--export-release`;
- deixar de copiar o executável completo do editor;
- confirmar título sem `(DEBUG)`;
- falta validar resize e escala Windows;
- falta confirmar duas contas e partida pendente após restart;
- falta testar bundle e restauro do backup num segundo computador.

Não publicar este ZIP como release final enquanto o [[04-Migracao-Godot/checklist-cutover|checklist de cutover]] estiver em `ADIAR`.

## Haskell Windows - rollback

### Estrutura

```text
ImmutableTowers/
  immutable-towers.exe
  libfreeglut.dll
  run-game.bat
  run-game.ps1
  app/
    imagens/
      *.bmp
```

### Criar

```powershell
cabal build -O2
.\scripts\package-windows-release.ps1
```

Durante desenvolvimento:

```powershell
.\run-game.ps1
```

Gloss depende de OpenGL/GLUT. O bundle deve incluir as DLL necessárias e preservar `app/imagens` enquanto o launcher Haskell depender da estrutura histórica.

## Procedimento de release

1. executar Haskell e Godot tests;
2. regenerar catálogo/fixtures quando o domínio Haskell mudar;
3. criar o bundle numa pasta limpa;
4. extrair o ZIP para outra pasta;
5. iniciar e percorrer menu/conta/partida;
6. criar progresso, fechar e reabrir;
7. exportar a conta;
8. restaurar num armazenamento isolado;
9. confirmar que o executável não contém `(DEBUG)` e não é o editor;
10. confirmar que `tests/` e `tools/` não entram no pacote;
11. calcular e registar tamanho/hash;
12. testar num segundo computador;
13. atualizar diário, matriz e checklist.

## Outras plataformas

Linux e macOS não têm bundle validado nesta fase. Godot facilita exports futuros, mas cada plataforma exige preset, templates, permissões, teste nativo e pacote próprio. Não declarar suporte sem esse processo.
