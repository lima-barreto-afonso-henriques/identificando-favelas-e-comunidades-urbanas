# ==============================================================================
# IBGE SOCIAL — OPÇÃO 1: INTERPOLAÇÃO POR ÁREA PURA (BASELINE)
# Objetivo: transferir as contagens absolutas do Censo (setor censitário) para
# o grid H3 supondo DENSIDADE UNIFORME dentro do setor — o peso de cada
# hexágono é proporcional apenas à fração de área geométrica que ele ocupa
# dentro do setor (st_interpolate_aw, extensive = TRUE).
#
# LIMITAÇÃO CONHECIDA: setores mistos (parte formal, parte favela) têm os
# indicadores sociais "borrados" igualmente entre as duas porções — é
# exatamente a suposição de heterogeneidade que o artigo quer superar. Este
# script serve de BASELINE de comparação para as opções 2 (dasimetria por
# edificação) e 3 (picnofilático de Tobler), não como método recomendado.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. FUNÇÃO PRINCIPAL
# ==============================================================================
# Interpola indicadores sociais do Censo para a grade H3 via área pura.
#
# grid_proj         sf do grid H3, projetado (ex.: EPSG:31983), com coluna id_hex
# setores_proj      sf dos setores censitários, projetado, com coluna
#                   code_tract e as colunas de contagem absoluta (water_rede,
#                   water_total, ...)
# colunas_contagem  vetor com os nomes das colunas de contagem absoluta
#
# retorna um tibble com id_hex + as contagens interpoladas + diagnóstico

interpolar_area_pura <- function(grid_proj, setores_proj, colunas_contagem) {
  # falha cedo se a base de setores não tiver as colunas esperadas, em vez
  # de seguir com NA silencioso mais adiante
  faltantes <- setdiff(colunas_contagem, names(setores_proj))
  if (length(faltantes) > 0) {
    stop(
      "Colunas de contagem ausentes em 'setores_proj': ",
      paste(faltantes, collapse = ", "),
      ". Verifique se os setores censitários já têm as contagens absolutas ",
      "do Censo acopladas (script de preparação do Censo, Seção 3-4)."
    )
  }

  setores_proj <-
    setores_proj |>
    mutate(across(all_of(colunas_contagem), ~ coalesce(.x, 0)))

  # interpolação por peso de área (Areal Weighting): distribui cada
  # contagem do setor entre os hexágonos proporcionalmente à área de
  # sobreposição, preservando o total original
  message("[Área pura] Executando interpolação por peso de área...")
  interp_resultado <-
    st_interpolate_aw(
      setores_proj[colunas_contagem],
      to = grid_proj,
      extensive = TRUE
    )

  # recupera as chaves id_hex via junção espacial por centroide — mais
  # rápido e seguro do que confiar na ordem das linhas devolvida pelo
  # st_interpolate_aw
  dados_interpolados <-
    interp_resultado |>
    st_centroid() |>
    st_join(select(grid_proj, id_hex), join = st_intersects) |>
    st_drop_geometry() |>
    select(id_hex, all_of(colunas_contagem))

  resultado <-
    grid_proj |>
    st_drop_geometry() |>
    select(id_hex) |>
    left_join(dados_interpolados, by = "id_hex") |>
    mutate(across(all_of(colunas_contagem), ~ coalesce(.x, 0)))

  # checagem de preservação de massa: a soma interpolada por hexágono deve
  # bater com a soma original por setor (a menos de hexágonos fora de
  # qualquer setor, ex.: área rural). Diferenças grandes (> 2%) indicam
  # bug na junção espacial, não erro numérico esperado do método
  for (coluna in colunas_contagem) {
    total_original <- sum(setores_proj[[coluna]], na.rm = TRUE)
    total_interpolado <- sum(resultado[[coluna]], na.rm = TRUE)
    erro_pct <- if (total_original > 0) {
      abs(total_interpolado - total_original) / total_original * 100
    } else {
      NA_real_
    }

    if (!is.na(erro_pct) && erro_pct > 2) {
      warning(sprintf(
        "[Área pura] Preservação de massa falhou para '%s': original = %.0f, interpolado = %.0f (erro de %.1f%%). Verifique a junção espacial.",
        coluna,
        total_original,
        total_interpolado,
        erro_pct
      ))
    } else {
      message(sprintf(
        "[Área pura] OK — '%s': erro de preservação de massa = %.2f%%",
        coluna,
        ifelse(is.na(erro_pct), 0, erro_pct)
      ))
    }
  }

  resultado
}


# ==============================================================================
# 2. EXECUÇÃO (SE RODADO DIRETAMENTE, NÃO VIA source())
# ==============================================================================

if (sys.nframe() == 0) {
  grid_base <- readRDS(file.path(pasta_saida, "grid_dasimetrico.rds"))
  setores_ibge <- readRDS(file.path(pasta_saida, "ibge_censo.rds"))

  grid_proj <-
    grid_base |>
    st_as_sf() |>
    st_transform(crs = 31983) |>
    st_make_valid()

  setores_proj <-
    setores_ibge |>
    st_transform(crs = 31983) |>
    st_make_valid()

  # colunas de contagem absoluta geradas no script de preparação do Censo
  colunas_contagem <- c(
    "water_rede",
    "water_total",
    "sewer_rede",
    "sewer_total",
    "waste_rede",
    "waste_total",
    "sidewalk_rede",
    "sidewalk_total",
    "trees_rede",
    "trees_total"
  )

  resultado_bruto <-
    interpolar_area_pura(grid_proj, setores_proj, colunas_contagem)

  # recalcula as proporções finais a partir das contagens interpoladas
  ibge_social_area_pura <-
    resultado_bruto |>
    mutate(
      households_connected_to_water_supply_network = if_else(
        water_total > 0,
        water_rede / water_total,
        0
      ),
      households_connected_to_sewerage_system = if_else(
        sewer_total > 0,
        sewer_rede / sewer_total,
        0
      ),
      households_solid_waste_collection = if_else(
        waste_total > 0,
        waste_rede / waste_total,
        0
      ),
      households_with_sidewalk = if_else(
        sidewalk_total > 0,
        sidewalk_rede / sidewalk_total,
        0
      ),
      households_with_trees = if_else(
        trees_total > 0,
        trees_rede / trees_total,
        0
      )
    ) |>
    select(
      id_hex,
      households_connected_to_water_supply_network,
      households_connected_to_sewerage_system,
      households_solid_waste_collection,
      households_with_sidewalk,
      households_with_trees
    )

  glimpse(ibge_social_area_pura)

  saveRDS(
    ibge_social_area_pura,
    file.path(pasta_saida, "ibge_social_area_pura.rds")
  )

  message(">>> [Opção 1] Interpolação por área pura concluída e salva.")
}

# FIM ==========================================================================
