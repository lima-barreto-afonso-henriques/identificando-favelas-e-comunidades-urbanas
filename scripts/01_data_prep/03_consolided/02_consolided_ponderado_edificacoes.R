# ==============================================================================
# CONSOLIDAÇÃO DO GRID HEXAGONAL — VERSÃO PONDERADA POR EDIFICAÇÕES
#
# Objetivo: unir, por id_hex, todas as bases já processadas nos scripts
# anteriores em um único dataset hexagonal — o mesmo "dataset final" da
# versão de área pura (01_consolided_pura.R), mas usando a dasimetria
# PONDERADA POR EDIFICAÇÕES (script 08, Open Buildings como variável
# ancilar) para os indicadores sociais do Censo, em vez de área pura.
#
# ATENÇÃO — este script substitui a versão anterior que duplicava
# integralmente a função interpolar_dasimetrico_edificacoes() e o bloco de
# execução do script 08. Essa duplicação era um problema estrutural: ambos
# escreviam nos MESMOS arquivos de saída ('ibge_social_dasimetrico_
# edificacoes<sufixo>.rds' e 'diagnostico_pesos_edificacoes<sufixo>.rds'),
# então o resultado final dependia só de qual script rodou por último — e,
# na prática, causou pelo menos uma vez a sobrescrita silenciosa de uma
# correção já aplicada no script 08 pela versão desatualizada daqui.
#
# A partir de agora, a REGRA é: script 08 é a ÚNICA fonte da interpolação
# ponderada por edificações. Este script (02) só CONSOME a saída dele —
# nunca recalcula, nunca duplica a função. Se o script 08 mudar, este
# script não precisa de nenhuma alteração (só rodar o 08 de novo antes).
#
# Para a versão de área pura (baseline do artigo), ver '01_consolided_pura.R'.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))
source(here::here("scripts", "00_functions", "01_utils.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")


# ----------------------------------------------------------------------
# SELETOR DE RESOLUÇÃO
# ----------------------------------------------------------------------
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 11

pasta_saida <- here("data", "processed")
pasta_outputs <- here("outputs", "comparacao_dasimetria")

if (!dir.exists(pasta_outputs)) {
  dir.create(pasta_outputs, recursive = TRUE)
}

sufixo <- paste0("_res", resolucao_hex)

# ==============================================================================
# 1. IMPORTAR OS INGREDIENTES
# ==============================================================================
# cada bloco lê a saída de um script anterior do pipeline. Os nomes de
# arquivo abaixo são os REAIS gerados pelos scripts 01, 03, 04, 05, 06 e 08
# — não confundir com nomes de rascunho como '..._artigo.rds', que não
# correspondem a nenhuma saída existente no pipeline atual.
#
# Os ingredientes SEM relação com o Censo (grid base, OSM, Open Buildings,
# declividade) são os MESMOS da versão de área pura — o método de
# dasimetria só afeta a interpolação dos indicadores sociais do Censo.
# Só o bloco "indicadores sociais do Censo" muda em relação a
# 01_consolided_pura.R: aqui lemos a saída do script 08 (ponderada por
# edificações) em vez da saída de área pura.

# importar o grid hexagonal dasimétrico
arquivo_grid_dasimetrico <- file.path(
  pasta_saida,
  paste0("grid_dasimetrico", sufixo, ".rds")
)

grid_dasimetrico <-
  readRDS(arquivo_grid_dasimetrico) |>
  st_as_sf() |>
  rename(number_private_residences = num_residencias)
glimpse(grid_dasimetrico)

# importa os indicadores sociais do Censo
# (dasimetria PONDERADA POR EDIFICAÇÕES — saída do script 08, não recalculada
# aqui; se este arquivo não existir ou estiver desatualizado, rode o script
# 08 primeiro, para esta mesma resolução)
arquivo_ibge_censo_ponderado_edificacoes <- file.path(
  pasta_saida,
  paste0("ibge_social_dasimetrico_edificacoes", sufixo, ".rds")
)

if (!file.exists(arquivo_ibge_censo_ponderado_edificacoes)) {
  stop(
    "'",
    arquivo_ibge_censo_ponderado_edificacoes,
    "' não encontrado. Rode o script 08 (dasimetria ponderada por ",
    "edificações) para a resolução ",
    resolucao_hex,
    " antes de consolidar."
  )
}

ibge_censo_ponderado_edificacoes <-
  readRDS(arquivo_ibge_censo_ponderado_edificacoes)
glimpse(ibge_censo_ponderado_edificacoes)


# importa os indicadores de vulnerabilidade física (OSM)
# (mesmo ingrediente da versão de área pura — independe do método de
# dasimetria social)
arquivo_osm_data <- file.path(
  pasta_saida,
  paste0("open_street_maps", sufixo, ".rds")
)

osm_data <-
  readRDS(arquivo_osm_data)
glimpse(osm_data)

# importa os indicadores de morfologia urbana (Open Buildings)
# (mesmo ingrediente da versão de área pura)
arquivo_buildings_data <- file.path(
  pasta_saida,
  paste0("open_buildings", sufixo, ".rds")
)

buildings_data <-
  readRDS(arquivo_buildings_data)
glimpse(buildings_data)

# importando a declividade média (Topodata)
# (mesmo ingrediente da versão de área pura)
arquivo_topodata <- file.path(
  pasta_saida,
  paste0("topodata_declividade", sufixo, ".rds")
)

topodata <-
  readRDS(arquivo_topodata)
glimpse(topodata)

# importando a variável alvo (favelas e comunidades urbanas)
# ATENÇÃO: assumindo aqui que a variável alvo tem uma versão própria para
# esta variante ('..._ponderado_edificacoes<sufixo>.rds'), no mesmo padrão
# de nomenclatura do arquivo '..._area_pura<sufixo>.rds' usado em
# 01_consolided_pura.R. CONFIRME se esse arquivo existe de fato no seu
# 'data/processed' antes de rodar — se a classificação de favelas não
# depender do método de dasimetria social (por ex., se for calculada a
# partir de outra fonte, não do Censo interpolado), ajuste esta linha para
# apontar para o mesmo arquivo usado em 01_consolided_pura.R.
arquivo_favelas <- file.path(
  pasta_saida,
  paste0("favelas_e_comunidades_urbanas_ponderado_edificacoes", sufixo, ".rds")
)

if (!file.exists(arquivo_favelas)) {
  stop(
    "'",
    arquivo_favelas,
    "' não encontrado. Verifique se este é o nome correto do arquivo da ",
    "variável alvo para a variante ponderada por edificações — se a ",
    "classificação de favelas não depender do método de dasimetria social, ",
    "ajuste este caminho para reaproveitar o mesmo arquivo usado em ",
    "01_consolided_pura.R ('favelas_e_comunidades_urbanas_area_pura",
    sufixo,
    ".rds')."
  )
}

favelas_data <- readRDS(arquivo_favelas)
glimpse(favelas_data)


# ==============================================================================
# 2. CHECAGEM DE UNICIDADE DE id_hex, TABELA A TABELA
# ==============================================================================
# se algum ingrediente tiver id_hex duplicado, um left_join o multiplica
# silenciosamente (fan-out) — o hexágono aparece repetido no resultado
# final, inflando a contagem de linhas sem gerar nenhum erro visível. A
# checagem geral do fim do script (Seção 4) só avisaria que "algo" deu
# errado, sem apontar qual tabela foi a origem. Isolamos a causa aqui,
# antes que ela se propague pelos joins.

tabelas_para_checar <- list(
  grid_dasimetrico = grid_dasimetrico,
  ibge_censo = ibge_censo_ponderado_edificacoes,
  osm_data = osm_data,
  buildings_data = buildings_data,
  topodata = topodata,
  favelas_data = favelas_data
)

for (nome_tabela in names(tabelas_para_checar)) {
  tbl <- tabelas_para_checar[[nome_tabela]]
  n_dup <- nrow(tbl) - n_distinct(tbl$id_hex)
  if (n_dup > 0) {
    warning(sprintf(
      "'%s' tem %d linha(s) com id_hex duplicado ANTES do join — isso vai causar fan-out silencioso no left_join. Investigue esta tabela antes de prosseguir.",
      nome_tabela,
      n_dup
    ))
  }
}


# ==============================================================================
# 3. JOINS SEQUENCIAIS
# ==============================================================================
# parte sempre do grid completo (script 01), então nenhum hexágono é
# perdido — hexágonos sem correspondência em algum ingrediente recebem os
# valores-padrão já definidos no script de origem daquele ingrediente
# (0 para contagens, NA para médias/desvios, FALSE para interseções OSM)

grid_hexagonal <-
  grid_dasimetrico |>
  left_join(
    st_drop_geometry(ibge_censo_ponderado_edificacoes),
    by = "id_hex"
  ) |>
  left_join(osm_data, by = "id_hex") |>
  left_join(buildings_data, by = "id_hex") |>
  left_join(topodata, by = "id_hex") |>
  left_join(st_drop_geometry(favelas_data), by = "id_hex")

glimpse(grid_hexagonal)

skimr::skim(st_drop_geometry(grid_hexagonal))


# ==============================================================================
# 4. VALIDAÇÃO FINAL (SANITY CHECK)
# ==============================================================================
# verificação, não correção: se isto disparar, o problema está em algum dos
# ingredientes (Seção 2 deveria ter apontado qual) — não tente "consertar"
# aqui removendo duplicatas às cegas

assertthat::assert_that(
  n_distinct(grid_hexagonal$id_hex) == nrow(grid_hexagonal),
  msg = "id_hex duplicado após os joins! Verifique os avisos da Seção 2."
)


# ==============================================================================
# 5. SALVAR
# ==============================================================================

sufixo_saida <- paste0("_res", resolucao_hex)

saveRDS(
  grid_hexagonal,
  file.path(
    pasta_saida,
    paste0("grid_hexagonal_ponderado_edificacoes", sufixo_saida, ".rds")
  )
)

# FIM ==========================================================================
