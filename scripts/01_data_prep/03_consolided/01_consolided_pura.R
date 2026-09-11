# ==============================================================================
# CONSOLIDAÇÃO DO GRID HEXAGONAL — VERSÃO ÁREA PURA (ARTIGO)

# Objetivo: unir, por id_hex, todas as bases já processadas nos scripts
# anteriores em um único dataset hexagonal — o "dataset final" da Tabela 1
# do artigo, pronto para a análise descritiva e a modelagem. Usa a
# interpolação por ÁREA PURA (script 08) para as variáveis sociais do
# Censo, que é o método usado no artigo original.
#
# Para a versão alternativa (dasimetria ponderada por edificações), ver
# '14_consolidar_grid_hexagonal_ponderado_edificacoes.R'.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

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
# (interpolação por área pura)
arquivo_ibge_censo_area_pura <- file.path(
  pasta_saida,
  paste0("ibge_social_area_pura", sufixo, ".rds")
)

ibge_censo_area_pura <-
  readRDS(arquivo_ibge_censo_area_pura)
glimpse(ibge_censo_area_pura)


# importa os indicadores de vulnerabilidade física (OSM)
arquivo_osm_data <- file.path(
  pasta_saida,
  paste0("open_street_maps", sufixo, ".rds")
)

osm_data <-
  readRDS(arquivo_osm_data)
glimpse(osm_data)

# importa os indicadores de morfologia urbana (Open Buildings)
arquivo_buildings_data <- file.path(
  pasta_saida,
  paste0("open_buildings", sufixo, ".rds")
)

buildings_data <-
  readRDS(arquivo_buildings_data)
glimpse(buildings_data)

# importando a declividade média (Topodata)
arquivo_topodata <- file.path(
  pasta_saida,
  paste0("topodata_declividade", sufixo, ".rds")
)

topodata <-
  readRDS(arquivo_topodata)
glimpse(topodata)

# importando a variável alvo (favelas e comunidades urbanas)...")
arquivo_favelas <- file.path(
  pasta_saida,
  paste0("favelas_e_comunidades_urbanas_area_pura", sufixo, ".rds")
)

favelas_data <- readRDS(arquivo_favelas)
glimpse(favelas_data)


# ==============================================================================
# 2. CHECAGEM DE UNICIDADE DE id_hex, TABELA A TABELA
# ==============================================================================
# se algum ingrediente tiver id_hex duplicado, um left_join o multiplica
# silenciosamente (fan-out) — o hexágono aparece repetido no resultado
# final, inflando a contagem de linhas sem gerar nenhum erro visível. A
# checagem geral do fim do script (Seção 5) só avisaria que "algo" deu
# errado, sem apontar qual tabela foi a origem. Isolamos a causa aqui,
# antes que ela se propague pelos joins.

tabelas_para_checar <- list(
  grid_dasimetrico = grid_dasimetrico,
  ibge_censo = ibge_censo_area_pura,
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
  left_join(st_drop_geometry(ibge_censo_area_pura), by = "id_hex") |>
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
    paste0("grid_hexagonal_area_pura", sufixo_saida, ".rds")
  )
)

# FIM ==========================================================================
