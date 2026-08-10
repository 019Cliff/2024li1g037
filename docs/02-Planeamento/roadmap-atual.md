# Immutable Towers - Roadmap Atual

Tags: #roadmap #prioridade #apresentacao

Resumo curto do trabalho realmente aberto. A sequência e os critérios completos estão no [[plano-melhoria-total-2026-07-29|Plano de Melhoria Total]]. Evidência concluída fica em [[estado-atual]], na [[03-Qualidade-e-Release/auditoria-2026-07-29|Auditoria 2026-07-29]] e na [[04-Migracao-Godot/matriz-paridade|Matriz de Paridade]].

## Agora - consolidar e medir

1. usar o novo gate para equilibrar Desafio, Bosses e campanhas completas;
2. calibrar o Bot 3.0 com a memória e rollout já implementados;
3. provar que o arsenal completo supera Sentinela-only nos cinco mapas;
4. criar perfis de dificuldade por capítulo, estágio, mapa e modo;
5. repetir a matriz visual e de input em resoluções extremas.

## Próximo - Bot 3.0 e campanha

- afinar posicionamento, composição e reserva com o rollout curto;
- garantir que arsenal completo supera Sentinela-only;
- calibrar os perfis de dificuldade e assinaturas já ligados aos cinco estágios;
- equilibrar torres, Desafio e Bosses com métricas de resultado.

## Depois - apresentação e conteúdo

- centrar/reorganizar UI em 4:3, 16:9 e 21:9;
- escala de UI, teclado e acessibilidade;
- coleção e detalhe por torre;
- baús maiores, estados sem sobreposição e sequência de revelação refinada;
- modelos de torres por nível/especialização;
- telegraphs/modelos de inimigos e bosses;
- HUD com cards, ícones e preview de vaga;
- VFX com orçamento e áudio licenciado;
- testar `Contratos de Defesa` apenas depois do núcleo estar estável.

## Gates antes do cutover

- economia sem arbitragem: **APROVADO automaticamente**;
- nenhuma vaga finita ou boss preso: **APROVADO no gate atual**;
- bot completo melhor do que a estratégia Sentinela-only;
- campanha e modos com baselines aprovadas;
- conta Godot exportada e restaurada sem perda num segundo computador;
- matriz manual de resoluções/rácios e input;
- export Windows real sem `(DEBUG)`;
- bundle e transferência de conta confirmados num segundo computador;
- aprovação explícita para trocar o launcher.

## Estado da decisão

Cutover: **ADIAR**.

Continuar o desenvolvimento em Godot e manter Haskell como referência/rollback. A grelha global 12x11 está aprovada; os bloqueadores atuais são gameplay, bot e release, não a escala do mapa.
