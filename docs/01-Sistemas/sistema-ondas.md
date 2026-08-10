# Immutable Towers - Sistema Ondas

Tags: #sistema #ondas

Relacionadas: [[HOME]], [[estado-atual]], [[sistema-inimigos]], [[backlog-jogo]], [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]]

## Modulos

### Haskell

- `app/WaveSystem.hs`: `EnemyGroup`, `WavePlan`, composicoes e mutadores
- `app/ProgressionSystem.hs`: campanha, desbloqueios e total de vagas
- `app/Tempo.hs`: spawn automatico e geracao do infinito
- `app/UIState.hs`: resumo e composicao da proxima vaga

### Godot

- `godot/data/domain-catalog-v1.json`: vagas finitas e referências do infinito;
- `godot/src/domain/infinite_wave_generator.gd`: geração/mutadores;
- `godot/src/domain/enemy_scaling.gd`: escalamento compatível;
- `godot/src/domain/run_difficulty_profile.gd`: capítulo, estágio, assinatura do mapa e tempo de referência;
- `godot/src/domain/vertical_slice_simulation.gd`: spawn e progressão;
- `godot/src/presentation/vertical_slice_screen.gd`: barra e preview textual.

## Composicao

Uma `WavePlan` descreve grupos de classes e ciclo de spawn. Isto permite desenhar historia, desafio, boss e sandbox sem depender apenas de `indice mod n`.

- Historia: dez vagas por estagio, dificuldade guiada pelo capitulo/estagio.
- Infinito: uma vaga de cada vez, quantidade e nivel crescentes.
- Desafio: misturas que exigem counters diferentes.
- Boss: tres encontros com escoltas e bosses distintos.
- Sandbox: amostra das classes normais para teste.

### Perfis da campanha

O estágio 1 mantém exatamente as dez vagas de referência. Os restantes estágios são derivados de forma determinística:

- estágio 2 acrescenta a classe assinatura do mapa em vagas marcadas;
- estágio 3 substitui inimigos básicos por essa assinatura;
- estágio 4 introduz suporte e aperta ligeiramente o ciclo;
- estágio 5 acrescenta um Elite à vaga final;
- capítulos posteriores escalam vida, ataque, velocidade, loot e tempo de referência.

Assinaturas atuais: Rápido, Blindado, Regenerador, Dispersor e Protegido. O perfil e as vagas transformadas ficam no save.

## Mutadores do infinito

- `Fortificados`: a cada quatro vagas, mais vida e ataque.
- `RecompensaEscassa`: a cada seis vagas, menos butim.
- `OndaDupla`: a cada nove vagas, duplica a composicao e acelera o spawn.

Os marcos podem coincidir, criando combinacoes previsiveis. O HUD mostra uma notificacao quando um mutador entra.

## Recompensa do Infinito

- a derrota termina a run e paga gemas pela vaga efetivamente alcançada;
- a recompensa cresce rapidamente no início e abranda depois da vaga 15;
- o máximo é 188 gemas por partida, equivalente a um baú Imperial numa run de vaga 120;
- a melhor vaga alcançada fica guardada no perfil;
- fórmula, limites e exemplos estão em [[sistema-progressao#Recompensa do Infinito]].

## Preview

`UIState` agrega a composicao da proxima vaga numa passagem e o painel mostra ate tres classes com contagens. O calculo fica fora das funcoes de desenho de modelos e evita multiplas pesquisas por classe.

## Editor

Os cinco mapas continuam definidos no catálogo-fonte Haskell em 36x34. Antes de iniciar uma partida Godot, `WorldGrid` agrega blocos 3x3 numa grelha jogável 12x11, remapeia portal/base e volta a calcular a rota. Caminho e asfalto têm prioridade dentro do bloco para preservar conectividade; os cinco resultados são validados automaticamente. O editor trabalha diretamente na grelha global e migra documentos personalizados v1 maiores ao carregar.

Cada alteracao e validada antes de ser aplicada. O editor rejeita:

- base ou portal fora de Terra/Asfalto;
- terreno invalido sob uma torre;
- mapa sem caminho de qualquer portal ate a base.

## Qualidade

- ciclos de spawn sao limitados a valores positivos
- mutadores e composicoes possuem testes unitarios
- as primeiras 18 vagas infinitas coincidem com referências Haskell
- a gate limita tempo sem progresso e cobre os cinco estágios do capítulo 1
- os cinco estágios vencem na baseline automática; vida final 80, 80, 48,6, 47,8 e 80
- Desafio e Bosses terminam sem stalemate, mas ainda perdem na baseline e precisam de afinação
- falta playtest humano de ritmo, pausas e economia em vagas longas
- falta preview visual de counters e perigo
- o bot deve prever a vaga atual e duas seguintes ou uma janela temporal equivalente
- telemetria deve separar dano, economia e motivo de fuga por vaga

Ver [[02-Planeamento/plano-melhoria-total-2026-07-29#Fase 4 - Campanha, modos e balanceamento|Campanha, modos e balanceamento]].
