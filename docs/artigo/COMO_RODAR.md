# Como rodar este projeto — passo a passo

Este guia assume que você está abrindo o projeto pela primeira vez, numa
máquina que talvez nunca tenha rodado R, Quarto ou LaTeX antes. Siga na
ordem.

## 0. O que precisa estar instalado na máquina (uma vez, antes de tudo)

1. **R** — <https://cran.r-project.org>
2. **Quarto** — <https://quarto.org/docs/get-started/>
3. **Uma distribuição LaTeX** (o motor que monta o PDF por trás do Quarto).
   A forma mais simples é rodar, dentro do R:
   ```r
   quarto::quarto_install_tinytex()
   ```
   Isso evita instalar um LaTeX completo (que pesa vários GB).

Um editor ajuda, mas não é obrigatório: RStudio ou Positron.

## 1. Organizar a pasta do projeto

Coloque os arquivos exatamente nesta estrutura:

```
regressão/                          ← pasta do projeto (qualquer nome serve)
├── _quarto.yml
├── .here
├── .Rprofile                       ← criado pelo renv (ver Seção 3)
├── renv.lock                       ← criado pelo renv (ver Seção 3)
├── renv/
│   ├── activate.R                  ← criado pelo renv
│   └── settings.json               ← criado pelo renv
├── artigo_atualizado_v8.qmd
├── instalar_pacotes.R
├── referencias.bib
├── associacao-brasileira-de-normas-tecnicas.csl
└── R/
    └── helpers.R
```

**Por que a estrutura importa:** o `.qmd` localiza `R/helpers.R` através de
`here::here("R", "helpers.R")` — nunca por um caminho fixo tipo
`D:\usuario\projeto\R\helpers.R`. O pacote `here` descobre a raiz do
projeto encontrando o arquivo `.here` (ou o `_quarto.yml`) na pasta. Por
isso a pasta pode ser movida, renomeada, ou aberta em outro computador sem
quebrar nada — o caminho é sempre relativo à raiz, nunca absoluto.

Abra essa pasta no RStudio/Positron (ou apenas abra o editor **dentro**
dela), para garantir que o R enxergue a pasta certa como diretório de
trabalho.

## 2. Instalar os pacotes (só na primeira vez)

Com a pasta aberta, rode no console do R:

```r
source("instalar_pacotes.R")
```

Isso instala o `pacman` (se faltar) e usa `pacman::p_load()` para instalar
e carregar as ~20 dependências do projeto — desde manipulação de dados
(`tidyverse`), passando por modelos (`car`, `lmtest`, `sandwich`), até os
pacotes de econometria espacial que são o coração da análise (`spdep`,
`spatialreg`, `spgwr`). Pode demorar alguns minutos na primeira vez,
especialmente `sf` e `spdep` — eles dependem de bibliotecas geoespaciais
do sistema operacional (GDAL/GEOS/PROJ). Se der erro de compilação aqui,
é sempre nisso — a solução muda por sistema operacional (no Windows
geralmente resolve sozinho com o binário do CRAN; no Linux, é preciso
instalar essas libs via `apt` antes).

## 3. Travar as versões dos pacotes com `renv` (recomendado para entrega acadêmica)

### O problema que isso resolve

O mesmo código, rodado hoje e daqui a 8 meses, pode produzir resultados
ligeiramente diferentes — não por erro seu, mas porque os pacotes de R
mudam com o tempo (uma correção de bug no `spdep`, uma mudança de
comportamento padrão no `dplyr`, etc.). Reprodutibilidade científica
exige congelar não só o código, mas também o **ambiente** exato em que
ele rodou.

### Os comandos, um de cada vez

**`renv::init()`** — rodar uma vez, no início:
```r
renv::init()
```
Ele varre o projeto em busca de pacotes usados, cria uma **biblioteca de
pacotes isolada para este projeto** (pasta `renv/library/`, separada da
biblioteca "global" do seu R, que é compartilhada por todos os seus outros
projetos), copia as versões que você já tem instaladas para dentro dela, e
cria um `.Rprofile` que ativa tudo isso automaticamente sempre que você
abrir o R dentro desta pasta.

**`renv::snapshot()`** — "tira a foto" das versões instaladas:
```r
renv::snapshot()
```
Grava, num arquivo `renv.lock` (JSON legível), o nome, a **versão exata**
e a origem (CRAN, GitHub, etc.) de cada pacote — por exemplo, não apenas
"spdep", mas `spdep 1.3-6`. Rode este comando de novo sempre que instalar
ou atualizar algum pacote durante o desenvolvimento do trabalho — ele
nunca se atualiza sozinho.

**`renv::restore()`** — o comando que *o professor* roda:
```r
renv::restore()
```
Lê o `renv.lock` e instala exatamente as versões ali listadas, na
biblioteca local da máquina dele — sem tocar na biblioteca global dele,
sem risco de afetar outros projetos que ele tenha. A partir daí, o
`quarto render` na máquina dele usa o mesmo ambiente que o seu.

**`renv::status()`** — para checar se está tudo sincronizado:
```r
renv::status()
```
Avisa se você instalou algo novo e esqueceu de rodar `snapshot()`.

### O que entregar junto do trabalho

| Item | Entregar? |
|---|---|
| `renv.lock` | **Sim** — é o arquivo que importa |
| `.Rprofile` | **Sim** — ativa o renv automaticamente |
| `renv/activate.R` | **Sim** |
| `renv/settings.json` | **Sim** |
| `renv/library/` | **Não** — pacotes já compilados, pesados e específicos do seu sistema operacional; quem for rodar recria essa pasta sozinho com `renv::restore()` |

### O que o `renv` *não* resolve

- **A versão do R em si** — anote no seu trabalho qual versão usou
  (`R.version.string` no console).
- **Dependências de sistema** dos pacotes espaciais (`sf`, `spdep`
  dependem de GDAL/GEOS/PROJ instalados fora do R). Se a máquina que for
  rodar não tiver essas bibliotecas, o `renv::restore()` pode falhar ao
  tentar compilar o `sf` — isso não é um problema do `renv`, é uma
  limitação de qualquer pacote espacial em R nessa situação.

## 4. Renderizar pela primeira vez

No terminal (não no console do R — no terminal do sistema, ou no painel
"Terminal" do RStudio/Positron):

```bash
quarto render artigo_atualizado_v8.qmd
```

O que acontece, em ordem:

1. O Quarto lê o YAML do `.qmd` (título, formato PDF, bibliografia, os
   `params` de `data_url`/`seed`).
2. Executa o chunk `pacotes` (carrega o que foi instalado na Seção 2).
3. Executa o chunk `setup`, que roda
   `source(here::here("R", "helpers.R"))` — carregando as funções de
   formatação de tabela e gráfico.
4. Chega no chunk de importação de dados: como
   `data/pesquisa_od_zonas_2023.rds` **ainda não existe**, baixa o
   arquivo do GitHub e salva uma cópia local nesse caminho.
5. Roda, em ordem, cada chunk de análise (regressões, testes, modelos
   espaciais, GWR — os mais lentos são os espaciais e a GWR, que
   envolvem otimização numérica).
6. O `xelatex` (via `abntex2`) monta o PDF final, com a formatação ABNT.

Isso pode levar de 1 a alguns minutos, principalmente pela GWR (ela testa
várias larguras de banda por validação cruzada antes de escolher a
melhor).

## 5. Renderizações seguintes (o dia a dia de editar o texto)

Depois da primeira vez, dois mecanismos tornam tudo mais rápido:

- **Cache de dados**: como `data/pesquisa_od_zonas_2023.rds` já existe, o
  chunk de importação **não baixa de novo** — só lê o arquivo local. O
  documento passa a renderizar **offline** a partir daqui.
- **Freeze do Quarto** (`execute: freeze: auto`, no `_quarto.yml`): guarda
  o resultado de cada chunk numa pasta `_freeze/`, criada automaticamente.
  Se você editar só o *texto* (sem tocar em código), o Quarto percebe que
  os chunks não mudaram e reaproveita os resultados prontos, sem re-rodar
  os modelos espaciais de novo.

No dia a dia, editar o texto e rodar `quarto render` de novo deve ser
rápido. Só fica lento de novo se você editar um chunk de código (aí
apenas aquele chunk, e os que dependem dele, são re-executados).

## 6. Quando forçar algo do zero

| Situação | O que fazer |
|---|---|
| Os dados da Pesquisa OD foram atualizados na fonte | Apague `data/pesquisa_od_zonas_2023.rds` e rode `quarto render` de novo |
| Mudou um modelo e quer garantir que tudo recalcule, ignorando o freeze | `quarto render artigo_atualizado_v8.qmd --execute` |
| Mudou de computador / a pasta `_freeze/` não existe mais | Não precisa fazer nada — ela é recriada sozinha no próximo render (só fica mais lento nessa primeira vez) |

## 7. Erros mais comuns

- **`could not find function "here"`** (ou similar, logo no início) → a
  Seção 2 (instalar pacotes) não rodou, ou não terminou.
- **Erro de compilação do `sf`/`spdep`** → falta uma dependência de
  sistema (GDAL/GEOS/PROJ) — ver Seção 2.
- **`xelatex not found`** → falta o passo 0.3 (TinyTeX).
- **Alguma tabela sai "quebrada" no PDF** → normalmente sinal de um
  pacote do `kableExtra`/`booktabs` desatualizado; travar as versões
  cedo com `renv::snapshot()` (Seção 3) evita esse tipo de surpresa no
  futuro.

## Parâmetros do documento (`params`, no início do `.qmd`)

| Parâmetro | Uso |
|---|---|
| `data_url` | URL de origem da Pesquisa OD (só é lida se o cache local não existir) |
| `cache_path` | Caminho do cache local dos dados |
| `seed` | Semente única do documento (usada no jitter da Figura 1) |
