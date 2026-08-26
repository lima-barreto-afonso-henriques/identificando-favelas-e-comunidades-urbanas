# Classificação de Áreas de Favela em São Paulo

Pipeline geoespacial em R para identificação probabilística de áreas de
favela na Região Metropolitana de São Paulo, replicando a metodologia de
Nascimento et al. (2025) e documentando com transparência as extensões
propostas além do artigo original.

---

## 📖 Artigo de referência

Este projeto segue, etapa a etapa, a metodologia publicada em:

> Nascimento, G. A., Giannotti, M., Regueira, T. A., & Tomasiello, D. B.
> (2025). Identifying slum areas: A multidimensional analysis leveraging
> with explanatory machine learning techniques. *Sustainable Cities and
> Society*, 131, 106645.
> [doi.org/10.1016/j.scs.2025.106645](https://doi.org/10.1016/j.scs.2025.106645)

A regra do repositório é simples: **a linha de base segue o artigo à
risca** — mesmos dados, mesmo desenho de grid, mesmo modelo, mesmos
limiares testados no Apêndice A. Qualquer etapa em que este projeto vai
além do artigo (ver [Extensões](#-extensões-além-do-artigo)) está marcada
explicitamente no próprio código e nesta documentação, para que a
comparação entre "replicação" e "proposta própria" fique sempre clara.

---

## 🗂️ Estrutura do repositório

> Estrutura de referência — ajuste os nomes de pasta abaixo conforme a
> organização real do seu repositório, se tiver divergido durante o
> desenvolvimento.

```
├── scripts/
│   ├── 00_functions/         # código compartilhado por todos os scripts
│   │   ├── 00_setup.R        # pacotes e opções globais
│   │   ├── 01_utils.R        # funções utilitárias (estatística, I/O)
│   │   └── 02_themes.R       # paleta de cores e temas ggplot2 do projeto
│   │
│   ├── 01_preprocessing/     # ingestão, grid H3, mapeamento dasimétrico
│   ├── 02_eda/               # análise exploratória (distribuições, correlação)
│   ├── 03_analysis/          # análise de sensibilidade do limiar (Apêndice A)
│   ├── 04_modeling/          # treino, avaliação, SHAP
│   └── 05_clustering/        # heterogeneidade das áreas classificadas (k-means)
│
├── data/
│   ├── raw/                  # dados brutos (não versionados)
│   ├── processed/            # grid hexagonal consolidado (.rds)
│   └── outputs/              # artefatos gerados pelos scripts (.rds, figuras)
│
└── README.md
```

---

## 🧭 Metodologia — etapa a etapa, espelhando o artigo

| Seção do artigo | Etapa | O que é feito |
|---|---|---|
| 3.1–3.2 | **Dados** | CNEFE (domicílios), Censo IBGE 2022 (infraestrutura), Favelas e Comunidades IBGE (rótulo), Open Buildings (edificações), OSM (rodovias/ferrovias/energia/hidrografia), Topodata (declividade) |
| 3.3 | **Pré-processamento** | Agregação em grid hexagonal H3, resolução 10; mapeamento dasimétrico (exclusão de áreas rurais/não habitadas) |
| 3.4 | **Análise descritiva** | Distribuições por classe (Slum × Non-Slum), teste de Shapiro-Wilk, teste *t*, correlação de Spearman com remoção de pares > 0,90 |
| 3.3–3.5 (Apêndice A) | **Sensibilidade do limiar** | Varredura de cobertura de 10% a 70%, Decision Tree + SMOTENC, 100-fold CV, teste de Mann-Whitney entre limiares (reprodução da Tabela A.4 e Fig. A.12) |
| 3.5 | **Classificação** | Split 80/20, SMOTENC (razão 50%), comparação de modelos, seleção do melhor (Random Forest, no artigo) |
| 3.6 | **Avaliação** | Matriz de confusão, recall/precisão/F1/ROC AUC, avaliação qualitativa por inspeção visual |
| 3.7 | **Importância de variáveis** | SHAP (SHapley Additive exPlanations) |
| 3.8 | **Heterogeneidade** | Clusterização k-means (k=3) das áreas classificadas como favela |

Cada etapa tem um script correspondente comentado com referências diretas
às seções e tabelas do artigo (ex.: `[F5] filtro de correlação > 0.9,
Seção 3.4`), para que qualquer decisão de modelagem seja rastreável até o
texto original.

---

## 🧪 Extensões além do artigo

O artigo usa split aleatório e CV aleatória de 10 folds — adequado para
muitos contextos, mas que pode inflar métricas quando há autocorrelação
espacial entre hexágonos vizinhos. Este projeto testa explicitamente até
que ponto isso importa, com três extensões documentadas:

- **Validação cruzada espacial** — blocos espaciais (`spatial_block_cv`)
  com buffer estimado por autocorrelação, em vez de amostragem aleatória,
  para verificar se as métricas do artigo se sustentam sob um desenho
  mais conservador.
- **Definição alternativa de cobertura, ponderada por edificações** — em
  vez de "% da área do hexágono coberta por polígono de favela" (o
  artigo), testa "% das edificações do hexágono dentro de polígono de
  favela", uma métrica potencialmente mais fiel à ocupação real.
- **Checagem de sensibilidade ao MAUP** (*Modifiable Areal Unit
  Problem*) — reagrega o mesmo grid para uma resolução H3 mais grosseira
  (via hierarquia nativa pai-filho do H3) e repete a sensibilidade de
  limiar em cada escala, para verificar se o limiar ótimo é uma
  propriedade dos dados ou um artefato do tamanho de hexágono escolhido.

Nenhuma dessas extensões substitui a replicação fiel — elas rodam **em
paralelo**, como scripts próprios, para que a linha de base do artigo
continue sempre disponível para comparação direta.

---

## ▶️ Como reproduzir

**Requisitos:** R ≥ 4.2, e os pacotes usados pelo projeto (instalados
automaticamente via `pacman::p_load()` no início de cada script):
`tidyverse`, `here`, `sf`, `tidymodels`, `themis`, `spatialsample`,
`blockCV`, `h3jsr`, `future`, `skimr`.

1. Clone o repositório e abra-o como projeto R (arquivo `.Rproj`, para que
   `here::here()` resolva os caminhos corretamente).
2. Coloque os dados brutos consolidados em `data/processed/` (o grid
   hexagonal já processado — ver `01_preprocessing/` para como ele é
   gerado a partir dos dados brutos).
3. Rode os scripts em ordem numérica dentro de cada pasta. Todos começam
   com `source(here::here("scripts", "00_functions", "00_setup.R"))`,
   que carrega pacotes e configurações compartilhadas.
4. Os artefatos (tabelas, modelos, figuras) são salvos em
   `data/outputs/` como `.rds`, prontos para os scripts seguintes ou para
   inspeção manual.

---

## ✍️ Autor

**Lima Barreto** — Universidade Federal do ABC (UFABC)

## 📄 Como citar este repositório

Se este código for útil para seu trabalho, cite também o artigo original
que fundamenta a metodologia:

```bibtex
@article{nascimento2025identifying,
  title   = {Identifying slum areas: A multidimensional analysis leveraging with explanatory machine learning techniques},
  author  = {Nascimento, Giovanni Attina do and Giannotti, Mariana and Regueira, Tiago Andrade and Tomasiello, Diego Bogado},
  journal = {Sustainable Cities and Society},
  volume  = {131},
  pages   = {106645},
  year    = {2025},
  doi     = {10.1016/j.scs.2025.106645}
}
```

## 📜 Licença

Defina aqui a licença do repositório (ex.: MIT, CC-BY-4.0) antes de
publicar.
