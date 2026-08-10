# Immutable Towers - Checklist de Cutover

Tags: #migracao #release #checklist

Relacionadas: [[matriz-paridade]], [[03-Qualidade-e-Release/distribuicao|Distribuição]], [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]

Estado atual: `ADIAR`.

## Baseline e integridade

- [x] migração, fixtures, exportador e vault protegidos por checkpoint Git revisto
- [x] Godot 4.7 importa o projeto sem erros
- [x] testes headless passam
- [x] fixtures Haskell/Godot de catalogo, ondas, bot, tiers, economia, combate, efeitos, movimento e base passam
- [x] vertical slice jogavel
- [x] cinco mapas e cinco modos presentes na simulacao
- [x] nove torres e onze classes presentes na simulacao
- [x] compra/upgrade/venda não permite arbitragem em nenhum modo
- [x] controlo não permite stalemates em vagas finitas ou bosses
- [x] score não recompensa tempo parado nem spam de torres
- [x] campanhas de resultado fazem gate de duração, vitória e progresso
- [x] copia de save real importada pela UI; origem Haskell manteve o SHA-256
- [x] duas contas independentes verificadas automaticamente
- [ ] duas contas verificadas manualmente depois de fechar/reabrir o bundle
- [x] conta Godot exportada e importada num armazenamento limpo sem perda
- [x] colisão de conta nunca substitui progresso sem confirmação
- [x] ação automática do bot persiste se o jogo fechar antes da vaga seguinte

## Produto e QA

- [x] UI validada em 1280x720, 1600x900, 1920x1080 e 2560x1440
- [x] UI validada também em 1366x768, 1280x800 e 21:9
- [ ] UI validada durante resize e com escala Windows 100%/125%/150%
- [ ] pausa, nova partida, vitória, derrota e continuar validados por interação
- [ ] cinco mapas, infinito 1x/4x e três bosses testados
- [ ] bot completo iguala ou supera Sentinela-only nos cinco mapas
- [ ] campanha usa dificuldade/composições próprias por estágio
- [x] nenhum texto visível diz scaffold, vertical slice, fatia vertical ou migração
- [x] bundle Windows de desenvolvimento extraído e iniciado fora do projeto
- [ ] export Windows release usa template de produção e não mostra `(DEBUG)`
- [ ] bundle e transferência de conta testados num segundo computador físico

## Decisão

- [ ] launcher principal alterado apenas apos aprovacao explicita

Rollback atual: continuar a usar `run-game.bat` e o bundle Haskell existente.

Plano para fechar os itens: [[02-Planeamento/plano-melhoria-total-2026-07-29|Plano de Melhoria Total]].
