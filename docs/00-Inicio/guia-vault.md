# Immutable Towers - Guia do Vault

Tags: #guia #vault #organizacao

Guia curto para manter o Obsidian consistente.

## Estrutura de pastas

- `00-Inicio`: Home, estado atual e este guia.
- `01-Sistemas`: documentacao tecnica por sistema do jogo.
- `02-Planeamento`: roadmap, backlog e funcionalidades concluidas.
- `03-Qualidade-e-Release`: testes, feedback, changelog e distribuicao.
- `04-Migracao-Godot`: arquitetura, paridade, saves, diario e cutover.
- `90-Referencia`: prompts antigos e especificacoes de apoio.

Cada pasta tem uma nota `Indice-*`. A entrada principal continua a ser [[HOME]].

## Convencao de nomes

Usar este padrao nos titulos:

- `Immutable Towers - Home`
- `Immutable Towers - Estado Atual`
- `Immutable Towers - Roadmap Atual`
- `Immutable Towers - Sistema UI`

Para nomes de ficheiro:

- `HOME`, `estado-atual`, `roadmap-atual`
- `backlog-jogo`, `checklist-regressao`, `template-playtest`
- `sistema-ui`, `sistema-torres`, `sistema-inimigos`, `sistema-ondas`, `sistema-progressao`

Nao repetir a mesma informacao em varias notas. A `HOME` aponta para os indices, o estado descreve o que existe e o backlog guarda apenas trabalho aberto.

## Separar facto, plano e histórico

- [[estado-atual]]: resumo vivo e curto;
- [[03-Qualidade-e-Release/auditoria-2026-07-29|auditoria]]: evidência datada, comandos e descobertas;
- [[02-Planeamento/plano-melhoria-total-2026-07-29|plano]]: sequência, dependências e critérios;
- [[backlog-jogo]]: tarefas abertas e acionáveis;
- [[roadmap-atual]]: versão curta/apresentável;
- `90-Referencia`: prompts históricos, nunca fonte de verdade atual.

Quando Haskell e Godot diferirem, criar subtítulos explícitos `Haskell` e `Godot`. Não escrever “o jogo” se a afirmação só for verdadeira numa das versões.

## Tags simples por tipo de nota

- `#hub`: paginas de entrada
- `#estado`: estado atual
- `#roadmap`: direcao e prioridades
- `#backlog`: trabalho em aberto
- `#feedback`: feedback de playtest
- `#sistema`: documentacao tecnica
- `#qa`: validacao, checklist e testes
- `#release`: distribuicao e bundle
- `#template`: modelos reutilizaveis
- `#changelog`: mudancas jogaveis
- `#arquivo`: notas antigas ou de apoio

## Regra pratica

Cada nota deve ter:

1. titulo consistente
2. linha de tags no topo
3. links para notas relacionadas
4. um objetivo claro

## Onde criar notas

- nova feature ou arquitetura: `01-Sistemas`
- ideia ou tarefa ainda aberta: `02-Planeamento`
- playtest, bug reproduzido ou release: `03-Qualidade-e-Release`
- trabalho especifico da migracao: `04-Migracao-Godot`
- prompt que nao representa o estado atual: `90-Referencia`
