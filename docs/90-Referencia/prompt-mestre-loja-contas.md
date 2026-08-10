# Prompt Mestre — Loja completa e contas locais persistentes

Tags: #prompt #loja #contas #persistencia #ui #roadmap

Relacionadas: [[HOME]], [[estado-atual]], [[backlog-jogo]], [[sistema-progressao]], [[sistema-ui]], [[sistema-torres]], [[prompt-mestre-design-visual]]

> Estado: especificação histórica da fase Haskell. O Godot já integrou contas locais, round-trip de backup, loja, recompensa e partidas por conta. Usar [[02-Planeamento/plano-melhoria-total-2026-07-29#6.3 Loja de baús|Loja de baús]] e [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 7 - Progressão, conta e retenção saudável|Contas/progressão]] como fonte atual.

> Copiar a secção “Prompt” para o modelo/agente que vai implementar esta ronda. O âmbito assumido é conta local no mesmo computador. Sincronização entre computadores exigiria um backend e não faz parte deste prompt.

## Diagnóstico de origem

- Existe apenas um perfil global editável; mudar `nomeJogador` não cria nem troca uma conta.
- Perfil, leaderboard, modo e `MetaProgress` são serializados em `immutable-towers-meta.txt`.
- A partida manual é serializada separadamente em `immutable-towers-save.txt`.
- Como os caminhos são globais, todos os jogadores da mesma instalação partilham gemas, desbloqueios, campanha, ranking e save.
- A loja meta atual é sobretudo texto: teclas `1`, `2`, `3` abrem baús e `F` faz a fusão da Tempestade.
- A abertura do baú não possui uma sequência visual própria, confirmação clicável nem ecrã de recompensa.
- O fallback de duplicados menciona conversão em ouro, mas `MetaProgress` não contém ouro; dados, mensagem e economia não estão alinhados.
- Já existem `TowerSpec`, modelos procedurais das torres, raridades, `VisualTheme` e ícones Gloss que devem ser reutilizados.

## Prompt

Trabalha diretamente no repositório **Immutable Towers** como programador Haskell sénior, UI/UX designer e arquiteto de persistência. Implementa duas funcionalidades ligadas entre si:

1. uma loja meta completa, visual, clicável e animada;
2. um sistema de contas locais em que cada conta conserva todo o seu progresso e a sua partida guardada depois de fechar e reabrir o jogo.

Não entregues apenas um plano ou mockup. Inspeciona o código, implementa por fases, escreve testes, executa o jogo quando possível e atualiza o vault. Mantém o projeto compilável no final de cada fase.

### 1. Baseline e proteção do trabalho existente

Antes de alterar qualquer ficheiro:

- lê `ImmutableTowers.hs`, `MetaTypes.hs`, `SaveSystem.hs`, `ProgressionSystem.hs`, `ScoreSystem.hs`, `TowerSystem.hs`, `Eventos.hs`, `Tempo.hs`, `Desenhar.hs`, `UIRects.hs`, `VisualTheme.hs`, `UIIcons.hs` e os testes;
- lê `estado-atual`, `backlog-jogo`, `sistema-progressao`, `sistema-ui`, `sistema-torres` e `prompt-mestre-design-visual`;
- verifica `git status` e preserva alterações existentes que não sejam tuas;
- executa `cabal build` e `cabal test` e regista o número exato de testes;
- confirma como o executável termina e em que pontos o estado já é guardado;
- não assumes que o fecho da janela chama um callback de logout: progresso importante deve ser guardado no momento da transação ou num checkpoint seguro.

### 2. Limite do sistema de login

Implementa **contas locais**, não autenticação online:

- não pedir email;
- não prometer sincronização cloud;
- não prometer proteção contra alguém com acesso aos ficheiros do computador;
- permitir vários jogadores no mesmo PC;
- usar um `AccountId` interno estável e diferente do nome apresentado;
- permitir conta sem credencial ou PIN local opcional;
- chamar ao fluxo “Entrar”/“Terminar sessão”, deixando claro nas opções que os dados são locais.

Se implementares PIN/palavra-passe:

- nunca guardar texto simples;
- nunca usar MD5, SHA-1 ou SHA-256 direto como password hash;
- usar uma biblioteca mantida e compatível com o projeto que implemente Argon2id, scrypt ou PBKDF2 com salt único;
- documentar os parâmetros e a nova dependência;
- se não for possível integrar hashing adequado de forma fiável, remove a credencial e entrega contas locais selecionáveis sem fingir segurança.

Uma futura versão online deve ser tratada como outro projeto: backend, TLS, sessões, recuperação de conta, prevenção de abuso e cloud save. Não improvises isso dentro do cliente Gloss.

### 3. Modelo de dados de contas

Cria tipos dedicados, idealmente separados do estado de render:

```haskell
newtype AccountId = AccountId String

data LocalCredential
  = SemCredencial
  | PinProtegido PasswordHash

data AccountSummary = AccountSummary
  { accountIdSummary :: AccountId
  , nomeContaSummary :: String
  , nivelSummary :: Int
  , ultimoMapaSummary :: Maybe MapId
  }

data AccountData = AccountData
  { versaoConta :: Int
  , accountId :: AccountId
  , nomeConta :: String
  , credencialConta :: LocalCredential
  , perfilConta :: PerfilJogador
  , progressoConta :: MetaProgress
  , leaderboardConta :: [Pontuacao]
  , modoSelecionadoConta :: ModoJogoEscolhido
  , estadoLojaConta :: ShopProgress
  , partidaPendenteConta :: Maybe SavedRun
  }
```

Os nomes concretos podem mudar, mas estas regras são obrigatórias:

- `AccountId` identifica armazenamento e relações; o nome pode ser alterado sem perder dados;
- nomes são únicos por comparação normalizada para evitar `Tomas` e `tomas` como contas ambíguas;
- nunca usar diretamente o nome do jogador como caminho de pasta;
- os tipos persistidos têm versão explícita;
- dados derivados não precisam de ser persistidos;
- nenhuma conta pode observar ou alterar o `MetaProgress`, leaderboard ou save de outra.

### 4. O que pertence a cada conta

Guardar por conta:

- nome e identidade interna;
- jogos, vitórias, derrotas e melhor pontuação;
- leaderboard pessoal;
- gemas;
- nível;
- torres desbloqueadas e fundidas;
- capítulo, estágio e estágios concluídos;
- rotação de mapas;
- último modo selecionado;
- estado económico da loja e contador/semente persistente de recompensas;
- recompensa de baú pendente, se existir;
- partida manual ou checkpoint para “Continuar”.

Guardar globalmente, fora da conta:

- versão do índice de contas;
- lista/resumo de contas;
- última conta usada, apenas se a opção “lembrar conta” estiver ativa;
- opções de vídeo, áudio, efeitos e acessibilidade que façam sentido para a instalação inteira.

Se existir leaderboard global local, constrói-o a partir de entradas imutáveis com `AccountId` e nome mostrado. Não o confundas com o leaderboard pessoal.

### 5. Local de armazenamento e robustez

Deixa de depender apenas da pasta de execução. Usa a pasta de dados da aplicação, por exemplo através de `getAppUserDataDirectory "ImmutableTowers"`, adicionando apenas dependências pequenas e justificadas (`directory`/`filepath`, se necessário).

Estrutura recomendada:

```text
ImmutableTowers/
  accounts-index-v1.txt
  settings-v1.txt
  accounts/
    account-000001/
      profile-v1.txt
      run-v2.txt
    account-000002/
      profile-v1.txt
      run-v2.txt
```

Implementa escrita recuperável:

1. serializa para `.tmp`;
2. volta a ler e valida o conteúdo temporário;
3. roda o ficheiro válido anterior para `.bak`;
4. promove o temporário;
5. em erro, mantém o anterior e apresenta mensagem clara;
6. no arranque, tenta principal, depois backup;
7. nunca apaga silenciosamente progresso que não conseguiu ler.

Não afirmes atomicidade perfeita em todos os sistemas operativos sem a garantir; o objetivo mínimo é recuperação previsível após falha.

### 6. Migração sem perda de progresso

No primeiro arranque da nova versão:

- procura `immutable-towers-meta.txt` e `immutable-towers-save.txt` antigos;
- se ainda não existirem contas, oferece “Importar progresso anterior”;
- cria uma conta `Jogador importado` ou pede um nome;
- migra perfil, leaderboard, modo, `MetaProgress`, `Jogo` e `TowerRegistry`;
- valida os dados migrados antes de marcar a migração como concluída;
- conserva cópias `.legacy.bak` dos ficheiros antigos;
- regista um marcador de migração para não duplicar a conta em todos os arranques;
- se a migração falhar, permite tentar novamente e não sobrescreve a origem.

Escreve testes com conteúdo das versões antigas já suportadas por `SaveSystem`.

### 7. Fluxo de entrada e contas

O arranque deve seguir este fluxo:

```text
Arranque
  → carregar índice
  → sem contas: Boas-vindas / Criar conta / Importar progresso
  → com contas: Escolher conta
  → introduzir PIN, apenas se configurado
  → carregar toda a conta
  → menu principal
```

Cria modos/estados explícitos, por exemplo:

- `BoasVindas`;
- `SelecionarConta`;
- `CriarConta`;
- `DesbloquearConta AccountId`;
- `GerirConta`;
- `MenuInicial`.

Na seleção de conta mostra cartões com:

- avatar/símbolo procedural;
- nome;
- nível;
- capítulo/estágio;
- torres desbloqueadas;
- última atividade ou “novo jogador”;
- botão `ENTRAR`;
- ações secundárias `RENOMEAR`, `PIN`, `EXPORTAR`, `ELIMINAR`.

Regras de UX:

- criar conta com rato e teclado;
- foco visível no campo de texto;
- validação de nome antes de guardar;
- mensagens para nome vazio, duplicado ou demasiado longo;
- `ESC` volta sem criar dados parciais;
- eliminar conta exige confirmação forte escrevendo o nome ou confirmando duas vezes;
- antes de eliminar, oferecer exportação/backup;
- terminar sessão guarda o estado, limpa dados sensíveis e volta à seleção;
- trocar conta nunca reutiliza em memória o progresso da conta anterior.

### 8. Autosave e sessão

Cria uma única API de persistência para evitar chamadas espalhadas:

```haskell
saveActiveAccount :: AccountSession -> IO (Either SaveError ())
modifyAndPersistAccount
  :: (AccountData -> Either DomainError AccountData)
  -> AccountSession
  -> IO (Either AccountError AccountSession)
```

Guarda imediatamente após:

- criação/renomeação da conta;
- alteração de PIN;
- vitória ou derrota;
- recompensa de modo;
- abertura de baú;
- fusão/desbloqueio de torre;
- alteração importante de progresso;
- save manual da partida;
- logout.

Durante uma partida longa, usa checkpoints espaçados ou eventos importantes; não escreve a cada frame. Se a escrita falhar, mantém a sessão ativa, mostra erro persistente e permite tentar novamente.

### 9. Nova loja meta

Transforma `MostrarLojaMeta` num espaço visual completo com três separadores:

1. **BAÚS**;
2. **COLEÇÃO**;
3. **FUSÃO**.

Cabeçalho comum:

- nome/avatar da conta ativa;
- nível;
- gemas com ícone;
- progresso da coleção `n/9`;
- botão `VOLTAR`;
- tooltip curto para a economia.

Reutiliza `VisualTheme`, `UIIcons`, `TowerSpec`, raridades e modelos procedurais. Usa `UIRect` como fonte única de desenho e clique. Mantém atalhos de teclado como alternativa, mas a loja deve ser completamente utilizável com rato.

### 10. Separador de baús

Cria três cartões grandes e clicáveis:

#### Baú de Madeira

- silhueta baixa e larga;
- tábuas, cantos de ferro e fecho simples;
- animação idle muito subtil;
- comunicação de opção inicial/barata.

#### Baú de Cristal

- caixa angular com cristais laterais;
- brilho azul/ciano e reflexo curto;
- sensação de raridade intermédia.

#### Baú Imperial

- corpo escuro, moldura dourada e emblema/coroa;
- maior presença e pulso dourado lento;
- opção cara e de alto nível.

Cada cartão mostra:

- modelo animado;
- nome;
- custo;
- pool possível por raridade;
- hipótese/peso de cada categoria de recompensa, se houver aleatoriedade;
- estado `COMPRAR`, `SEM GEMAS` ou `COLEÇÃO COMPLETA`;
- feedback de hover, pressão e seleção por escala/contorno, não apenas cor.

Ao clicar:

1. abre confirmação com custo e gemas restantes;
2. valida novamente o saldo no domínio;
3. calcula uma única recompensa;
4. aplica débito e recompensa como uma transação pura;
5. guarda a conta com uma `PendingReward`;
6. só depois inicia a animação;
7. ao concluir a revelação, marca a recompensa como vista e volta a guardar.

Se o jogo fechar durante a animação, o próximo login deve mostrar a recompensa pendente, sem cobrar outra vez e sem duplicar o prémio.

### 11. Animação de abertura

Modela a animação como máquina de estados, não como vários booleanos:

```haskell
data ChestAnimation
  = ChestIdle
  | ChestConfirming ChestType
  | ChestShake ChestType Tempo
  | ChestUnlock ChestType Tempo
  | ChestBurst ChestType Tempo
  | RewardReveal ShopReward Tempo
  | RewardAwaitingAcknowledge ShopReward
```

Sequência alvo:

- hover: levantamento de 4–8 unidades e brilho de contorno;
- confirmação: botão comprime 100–160 ms;
- shake: 300–450 ms, movimento pequeno;
- fecho/runa: 200–300 ms;
- abertura: tampa ou painéis separam-se;
- burst: 250–450 ms com poucas partículas Gloss;
- reveal: silhueta da torre, nome, raridade, `NOVA` ou compensação;
- confirmação: `CONTINUAR` e entrada direta na coleção.

Adiciona modo de efeitos reduzidos que elimina shake e partículas fortes, mantendo a sequência e a informação.

### 12. Recompensas e duplicados

Corrige a incoerência atual de “ouro” inexistente. Escolhe e implementa uma política única:

- recomendado: duplicado converte numa quantidade explícita de gemas inferior ao custo do baú; ou
- introduz fragmentos apenas se também implementares completamente saldo, UI, uso e migração.

Não cries uma moeda que só aparece numa mensagem. A função de compra deve devolver algo explícito:

```haskell
data ShopReward
  = NovaTorre TowerId
  | CompensacaoDuplicado TowerId Int
  | RecompensaColecaoCompleta Int
```

Persiste um contador/semente por conta. Fechar o jogo antes de abrir não deve permitir repetir indefinidamente até sair a torre desejada. Mantém a lógica determinística e testável quando possível.

### 13. Separador de coleção

Mostra as nove torres numa grelha:

- modelo procedural em idle;
- nome, raridade e papel;
- bloqueada, descoberta, desbloqueada ou fundida;
- progresso total;
- filtros simples por raridade/papel apenas se melhorarem a navegação.

Ao selecionar uma torre abre detalhe com:

- modelo maior animado;
- descrição e função;
- dano, alcance, rajada, ciclo e efeito base;
- alvo prioritário;
- nível máximo e especializações;
- origem de desbloqueio;
- botão contextual para voltar, testar em Sandbox ou ir à fusão.

Não uses apenas uma torre cinzenta recolorida. Reutiliza os modelos finais da passagem visual e garante que cada silhueta é distinta.

### 14. Separador de fusão

Transforma a fusão da Tempestade num ritual visual compreensível:

- slot Tesla;
- slot Solar;
- custo em gemas;
- núcleo/slot Tempestade ao centro;
- requisitos completos/incompletos com ícone e texto;
- botão `FUNDIR` desativado quando faltar algo;
- confirmação antes da transação;
- animação em que energia das duas torres converge para o novo modelo;
- revelação final e atualização imediata da coleção.

A transação de fusão segue as mesmas garantias do baú: validar, aplicar uma vez, persistir antes da celebração, recuperar corretamente após interrupção.

### 15. Integração visual

Segue [[prompt-mestre-design-visual]] e a implementação visual que já existir:

- tema verde-floresta escuro e dourado envelhecido;
- modelos procedurais coesos;
- contraste suficiente;
- ícone + texto para informação crítica;
- animações curtas e determinísticas;
- nenhum asset comercial copiado;
- nenhum clipart incompatível;
- orçamento de primitivas e partículas adequado a 60 FPS.

Adiciona modelos de baús e efeitos num módulo dedicado, por exemplo `ShopVisuals.hs`, e separa lógica pura da loja em `ShopSystem.hs`. Evita tornar `Desenhar.hs` e `Eventos.hs` ainda maiores.

### 16. Organização técnica sugerida

Estrutura possível:

```text
app/
  AccountTypes.hs
  AccountSystem.hs
  AccountStorage.hs
  AccountUI.hs
  ShopTypes.hs
  ShopSystem.hs
  ShopVisuals.hs
```

Responsabilidades:

- `AccountTypes`: tipos persistidos e versões;
- `AccountSystem`: validação, criação, login local, logout e domínio puro;
- `AccountStorage`: caminhos, leitura, backup, migração e escrita;
- `AccountUI`: cartões, formulários e gestão;
- `ShopTypes`: estado de UI, recompensa e animação;
- `ShopSystem`: compra/fusão puras e invariantes;
- `ShopVisuals`: modelos, cartões e animações.

Não forces estes nomes se criarem ciclos. O essencial é separar domínio, IO, estado de apresentação e desenho.

### 17. Invariantes obrigatórias

- gemas nunca ficam negativas;
- uma compra gera no máximo uma recompensa;
- nenhuma recompensa pode ser cobrada duas vezes;
- animação não decide nem recalcula o prémio;
- conta A nunca recebe dados da conta B;
- renomear conta não altera o seu `AccountId` nem o caminho;
- logout limpa a conta ativa da memória visível;
- erro de save não destrói o último ficheiro válido;
- migração antiga é idempotente;
- eliminar uma conta requer confirmação e não elimina outras;
- dados desconhecidos/corrompidos produzem erro recuperável, não reset silencioso;
- todos os custos, mensagens e moedas mostrados correspondem aos dados reais.

### 18. Testes obrigatórios

Adiciona testes para:

- criar primeira conta;
- rejeitar nome vazio e duplicado normalizado;
- renomear sem mudar `AccountId`;
- duas contas com gemas, campanha e torres diferentes;
- logout/login restaura exatamente a conta correta;
- save de partida fica associado apenas à conta ativa;
- principal corrompido recupera backup;
- migração de meta/save legado;
- migração executada duas vezes não duplica conta;
- compra sem gemas não altera estado;
- compra válida debita e recompensa uma vez;
- reward pendente sobrevive a reinício;
- reconhecer reward não volta a pagar;
- duplicado aplica a compensação real;
- coleção completa tem comportamento definido;
- fusão falha sem requisitos e é idempotente após sucesso;
- todas as hitboxes da loja ficam dentro dos painéis;
- navegação por rato, teclado, `ESC` e `ENTER`.

Para IO, usa diretórios temporários de teste. Não escreve testes na pasta real do utilizador.

### 19. Ordem de implementação

1. baseline e testes de caracterização do save atual;
2. tipos/versionamento de contas;
3. armazenamento por conta, backups e migração;
4. ecrãs de boas-vindas, criar e escolher conta;
5. integração de sessão, autosave, logout e continuar partida;
6. domínio transacional da loja e recompensas pendentes;
7. layout clicável da loja;
8. modelos e animações dos três baús;
9. coleção e detalhe de torre;
10. fusão animada;
11. acessibilidade, efeitos reduzidos e multirresolução;
12. limpeza, testes completos e documentação.

Depois de cada fase:

- executa build/testes;
- lista ficheiros e decisões;
- corrige regressões antes de avançar;
- preserva alterações não relacionadas do utilizador;
- atualiza o vault quando o estado real mudar.

### 20. Critérios de aceitação

O trabalho só fica concluído quando:

- um primeiro arranque permite criar ou importar uma conta;
- existem pelo menos duas contas independentes verificadas por teste;
- fechar/reabrir e entrar na mesma conta restaura todo o progresso;
- trocar de conta troca gemas, campanha, coleção, ranking e save;
- todos os ficheiros antigos suportados podem ser migrados sem perda;
- a loja inteira funciona com rato e teclado;
- os três baús têm modelos e animações diferentes;
- nenhuma animação pode perder ou duplicar recompensa;
- coleção e fusão mostram estado real e requisitos;
- duplicados não mencionam uma moeda inexistente;
- resolução mínima 1280x720 permanece utilizável;
- modo de efeitos reduzidos conserva informação;
- `cabal build` e `cabal test` passam;
- documentação, changelog, checklist e distribuição explicam contas e localização dos saves.

No final entrega: resumo, fluxo das contas, esquema de ficheiros, migrações realizadas, screenshots da loja/login, resultados de testes, checklist manual e limitações. Começa pela persistência e pelas invariantes; só depois liga animações, para que a loja bonita nunca possa perder progresso.

## Referência de segurança

Se a versão local tiver credencial, seguir uma solução de password hashing moderna em vez de inventar criptografia ou guardar texto simples:

- OWASP Password Storage Cheat Sheet — https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html
- OWASP Authentication Cheat Sheet — https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html
