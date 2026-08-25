# ==============================================================================
# CONSOLIDAÇÃO DO GRID HEXAGONAL — VERSÃO PONDERADA POR EDIFICAÇÕES
# Objetivo: mesma consolidação do script 13, mas usando a definição
# ALTERNATIVA em dois pontos, para permitir comparar o efeito da escolha
# metodológica sobre o dataset final:
#
#   1. Indicadores sociais do Censo interpolados via dasimetria ponderada
#      por edificações (script 09), em vez de área pura (script 08).
#   2. Rótulo (variável alvo) definido por fração de ÁREA CONSTRUÍDA dentro
#      do polígono de favela (is_slum_edif, do script 03), em vez de
#      fração de ÁREA GEOMÉTRICA (is_slum).
#
# Este dataset NÃO é o usado no artigo original — serve para testar se a
# escolha metodológica (Opção 1 vs. Opção 2, ver script 11) muda as
# conclusões do modelo de forma relevante.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

if (!requireNamespace("assertthat", quietly = TRUE)) {
  install.packages("assertthat")
}

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. IMPORTAR OS INGREDIENTES
# ==============================================================================

message("Importando o grid hexagonal dasimétrico...")
grid_dasimetrico <-
  readRDS(file.path(pasta_saida, "grid_dasimetrico.rds")) |>
  st_as_sf() |>
  rename(number_private_residences = num_residencias)
glimpse(grid_dasimetrico)

message(
  "Importando os indicadores sociais do Censo (dasimetria por edificações)..."
)
ibge_censo <- readRDS(file.path(
  pasta_saida,
  "ibge_social_dasimetrico_edificacoes.rds"
))
glimpse(ibge_censo)

message("Importando os indicadores de vulnerabilidade física (OSM)...")
osm_data <- readRDS(file.path(pasta_saida, "open_street_maps.rds"))
glimpse(osm_data)

message("Importando os indicadores de morfologia urbana (Open Buildings)...")
buildings_data <- readRDS(file.path(pasta_saida, "open_buildings.rds"))
glimpse(buildings_data)

message("Importando a declividade média (Topodata)...")
topodata <- readRDS(file.path(pasta_saida, "topodata_declividade.rds"))
glimpse(topodata)

message(
  "Importando a variável alvo ponderada por edificação (favelas e comunidades urbanas)..."
)
# a Opção 2 do rótulo (script 03) usa fração de ÁREA CONSTRUÍDA em vez de
# área geométrica — renomeamos aqui para 'slum_coverage_pct'/'is_slum',
# os mesmos nomes usados no restante do pipeline, para que este dataset
# alternativo seja um "drop-in" comparável ao da Seção 13 (área pura)
favelas_data <-
  readRDS(file.path(
    pasta_saida,
    "favelas_e_comunidades_urbanas_ponderado_edificacoes.rds"
  )) |>
  rename(
    slum_coverage_pct = slum_coverage_pct_edif,
    is_slum = is_slum_edif
  )
glimpse(favelas_data)


# ==============================================================================
# 2. CHECAGEM DE UNICIDADE DE id_hex, TABELA A TABELA
# ==============================================================================
# mesma lógica do script 13: evita fan-out silencioso no join, isolando
# qual ingrediente causaria o problema antes de ele se propagar

tabelas_para_checar <- list(
  grid_dasimetrico = grid_dasimetrico,
  ibge_censo = ibge_censo,
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

grid_hexagonal <-
  grid_dasimetrico |>
  left_join(st_drop_geometry(ibge_censo), by = "id_hex") |>
  left_join(osm_data, by = "id_hex") |>
  left_join(buildings_data, by = "id_hex") |>
  left_join(topodata, by = "id_hex") |>
  left_join(st_drop_geometry(favelas_data), by = "id_hex")

glimpse(grid_hexagonal)
skimr::skim(st_drop_geometry(grid_hexagonal))


# ==============================================================================
# 4. VALIDAÇÃO FINAL (SANITY CHECK)
# ==============================================================================

assertthat::assert_that(
  n_distinct(grid_hexagonal$id_hex) == nrow(grid_hexagonal),
  msg = "id_hex duplicado após os joins! Verifique os avisos da Seção 2."
)


# ==============================================================================
# 5. SALVAR
# ==============================================================================

saveRDS(
  grid_hexagonal,
  file.path(pasta_saida, "grid_hexagonal_ponderado_edificacoes_artigo.rds")
)

message(sprintf(
  "Grid hexagonal consolidado (ponderado por edificações): %d hexágonos, %d colunas. Salvo em 'grid_hexagonal_ponderado_edificacoes_artigo.rds'.",
  nrow(grid_hexagonal),
  ncol(st_drop_geometry(grid_hexagonal))
))

# FIM ==========================================================================
