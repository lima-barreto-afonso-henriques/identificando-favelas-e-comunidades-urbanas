# Determinantes Socioeconômicos e Espaciais da Motorização na RMSP

Artigo em Quarto (`.qmd` → PDF via `abntex2`/`xelatex`), com base na Pesquisa
Origem e Destino do Metrô de São Paulo (2023).

## Estrutura do projeto

```
.
├── _quarto.yml                  # config do projeto (raiz para here::here(), freeze)
├── .here                        # arquivo-sentinela vazio (garante a raiz do projeto)
├── artigo_atualizado_v8.qmd     # texto do artigo + chunks de análise
├── instalar_pacotes.R           # rodar 1x para instalar todas as dependências
├── R/
│   └── helpers.R                # toda a lógica reutilizável (tabelas, gráficos, dados)
├── referencias.bib              # bibliografia (não incluído neste pacote de edição)
├── associacao-brasileira-de-normas-tecnicas.csl
└── data/
    └── pesquisa_od_zonas_2023.rds  # cache local dos dados (criado no 1º render)
```

`R/helpers.R` é a **única** fonte da formatação de tabelas e gráficos do
documento: qualquer ajuste de estilo (fonte, largura de tabela, cores de
gráfico) deve ser feito lá, nunca em um chunk específico do `.qmd`.

O `.here` já vem incluído neste pacote de arquivos — é só manter todos os
arquivos na mesma pasta e `here::here()` funciona sem configuração adicional
(o `_quarto.yml` também é reconhecido como raiz de projeto, então o `.here`
é uma garantia redundante, mas inofensiva).

## Como reproduzir

1. Baixe todos os arquivos deste pacote para uma mesma pasta (mantendo a
   subpasta `R/`) e abra essa pasta como projeto no RStudio ou Positron.
2. (Opcional, mas recomendado) rode `instalar_pacotes.R` uma vez para
   instalar todas as dependências de uma vez:
   ```r
   source("instalar_pacotes.R")
   ```
   Depois, para fixar as versões instaladas:
   ```r
   renv::init()
   renv::snapshot()
   ```
   e versione o `renv.lock` resultante junto com este projeto.
3. Renderize:
   ```bash
   quarto render artigo_atualizado_v8.qmd
   ```
   No primeiro render, os dados são baixados do GitHub e salvos em
   `data/pesquisa_od_zonas_2023.rds`; renders seguintes leem esse cache
   local (mais rápido e funciona offline). Para forçar um novo download,
   apague esse arquivo.
4. `execute: freeze: auto` (em `_quarto.yml`) reaproveita chunks que não
   mudaram entre renders — a pasta `_freeze/` que ele cria deve ser
   versionada junto com o projeto (é o que garante que outra pessoa
   consiga reproduzir exatamente a mesma saída sem precisar re-rodar os
   modelos espaciais mais custosos).

## Configuração (`params`, no início do `.qmd`)

| Parâmetro     | Uso                                                          |
|---------------|---------------------------------------------------------------|
| `data_url`    | URL de origem da Pesquisa OD (só é lida se o cache não existir) |
| `cache_path`  | Caminho do cache local dos dados                              |
| `seed`        | Semente única do documento (usada no jitter da Figura 1)       |
