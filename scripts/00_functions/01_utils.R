# Funções auxiliares para evitar repetição de código
#
#
# ==============================================================================
# Funções utilitárias de uso geral (estatística e I/O), usadas principalmente
# pelo script de análise descritiva (01_eda.R). Cores e temas de ggplot2
# ficam à parte, em 02_themes.R.
# ==============================================================================

# --- Assimetria (skewness) ---------------------------------------------------
calc_skewness <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  mean((x - mean(x))^3) / (mean((x - mean(x))^2)^1.5)
}

# --- Formatação padronizada de p-valor em notação científica ----------------
formatar_pvalor <- function(p, digits = 2) {
  formatC(p, format = "e", digits = digits)
}

# --- Salvar gráfico com os parâmetros padrão do projeto ---------------------
# largura/altura/dpi padrão usados nas figuras descritivas do artigo; evita
# repetir os mesmos argumentos em cada chamada de ggsave() e garante que a
# pasta de destino exista
salvar_grafico_projeto <- function(
  plot,
  caminho_arquivo,
  width = 20,
  height = 12,
  dpi = 300
) {
  # Extrai apenas o caminho da pasta a partir do caminho completo do arquivo
  pasta <- dirname(caminho_arquivo)

  # Cria a pasta caso ela não exista
  if (!dir.exists(pasta)) {
    dir.create(pasta, recursive = TRUE)
  }

  # Salva o gráfico
  ggsave(
    filename = caminho_arquivo,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

# ------------------------------------------------------------------------------
# FUNÇÃO DE LEITURA DO OPEN BUILDINGS EM PARQUET PARTICIONADO VIA DUCKDB
# ------------------------------------------------------------------------------
# Lê os arquivos Parquet particionados do Open Buildings aplicando DOIS filtros
# diretamente no DuckDB, antes de qualquer dado trafegar para o R:
#
#   1) poda de partições por bounding box (lat_bin/lon_bin), como antes —
#      descarta a maior parte dos arquivos sem precisar abri-los;
#   2) recorte espacial EXATO com ST_Intersects (extensão `spatial` do
#      DuckDB) contra o polígono da área de estudo (`mascara_regiao_wkt`).
#
# Isso substitui o antigo fluxo "traz tudo dentro do bbox -> st_filter() no R"
# (seção 4 do script 06): agora o polígono exato já filtra dentro do DuckDB,
# então só chega ao R o que realmente está dentro da área de estudo. Reduz e
# muito a quantidade de dados materializados como objeto `sf` em memória.
#
# O TRY_CAST explícito nas colunas lat_bin e lon_bin previne o erro de Binder
# ("Cannot mix values of type VARCHAR and DECIMAL em cláusula BETWEEN").
# ------------------------------------------------------------------------------
ler_open_buildings_parquet <- function(
  pasta_parquet,
  bbox_wgs84,
  mascara_regiao_sf = NULL,
  tamanho_celula_particao = 0.1,
  margem_graus = 0.01
) {
  if (!dir.exists(pasta_parquet)) {
    stop(
      "Pasta Parquet particionada '",
      pasta_parquet,
      "' não encontrada. Rode o script de download do Open Buildings ",
      "(Seção 5), que agora inclui a conversão para Parquet."
    )
  }

  bbox_expandido <- bbox_wgs84 + c(-1, -1, 1, 1) * margem_graus

  lat_bin_min <- floor(bbox_expandido["ymin"] / tamanho_celula_particao) *
    tamanho_celula_particao
  lat_bin_max <- floor(bbox_expandido["ymax"] / tamanho_celula_particao) *
    tamanho_celula_particao
  lon_bin_min <- floor(bbox_expandido["xmin"] / tamanho_celula_particao) *
    tamanho_celula_particao
  lon_bin_max <- floor(bbox_expandido["xmax"] / tamanho_celula_particao) *
    tamanho_celula_particao

  con <- duckdb::dbConnect(duckdb::duckdb())
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # extensão espacial: precisa ser instalada uma única vez por ambiente
  # (fica cacheada localmente pelo DuckDB), depois só carregar (LOAD)
  # obs: dbExecute é exportado pelo DBI, não pelo pacote duckdb
  DBI::dbExecute(con, "INSTALL spatial;")
  DBI::dbExecute(con, "LOAD spatial;")

  usa_recorte_exato <- !is.null(mascara_regiao_sf)

  if (usa_recorte_exato) {
    # converte o polígono (já deve estar em WGS84 / crs 4326) para WKT,
    # que é passado como literal para o DuckDB reconstruir a geometria
    mascara_wkt <- sf::st_as_text(sf::st_geometry(mascara_regiao_sf)[[1]])

    filtro_espacial_exato <- sprintf(
      "AND ST_Intersects(
         ST_Point(longitude, latitude),
         ST_GeomFromText('%s')
       )",
      mascara_wkt
    )
  } else {
    filtro_espacial_exato <- ""
  }

  query <- sprintf(
    "SELECT longitude, latitude, area_in_meters, confidence
     FROM read_parquet('%s/**/*.parquet', hive_partitioning = true)
     WHERE TRY_CAST(lat_bin AS DOUBLE) BETWEEN %f AND %f
       AND TRY_CAST(lon_bin AS DOUBLE) BETWEEN %f AND %f
       AND latitude BETWEEN %f AND %f
       AND longitude BETWEEN %f AND %f
       %s",
    pasta_parquet,
    lat_bin_min,
    lat_bin_max,
    lon_bin_min,
    lon_bin_max,
    bbox_expandido["ymin"],
    bbox_expandido["ymax"],
    bbox_expandido["xmin"],
    bbox_expandido["xmax"],
    filtro_espacial_exato
  )

  edificacoes <- DBI::dbGetQuery(con, query) |>
    as_tibble() |>
    rename(building_area = area_in_meters) |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  message(sprintf(
    "Leitura via Parquet particionado (duckdb%s): %d edificações %s importadas do Open Buildings.",
    if (usa_recorte_exato) " + spatial ST_Intersects" else "",
    nrow(edificacoes),
    if (usa_recorte_exato) {
      "dentro do polígono exato da área de estudo"
    } else {
      sprintf("dentro do bounding box (+%.2f° de margem)", margem_graus)
    }
  ))

  return(edificacoes)
}

# ------------------------------------------------------------------------------
# FUNÇÃO DE JOIN RÁPIDO PONTO -> HEXÁGONO H3 (SEM st_join ESPACIAL)
# ------------------------------------------------------------------------------
# Alternativa ao st_join(open_buildings_regiao, grid_h3_sf, join = st_intersects)
# usado na seção 5 do script 06. Em vez de um join espacial ponto-a-polígono
# (custoso para volumes grandes de edificações), calcula o índice H3 de cada
# ponto diretamente na MESMA resolução do grid e faz um join por chave
# (id_hex), que é ordens de magnitude mais rápido.
#
# Requer o pacote `h3jsr` (wrapper do H3 da Uber). Os ids retornados por
# h3jsr já batem com os ids usados no grid_h3_sf, desde que o grid também
# tenha sido gerado com h3jsr/h3 na mesma resolução.
# ------------------------------------------------------------------------------
vincular_h3_rapido <- function(pontos_sf, resolucao_hex) {
  if (!requireNamespace("h3jsr", quietly = TRUE)) {
    stop(
      "Pacote 'h3jsr' não instalado. Instale com install.packages('h3jsr') ",
      "para usar o join rápido, ou use st_join() como alternativa mais lenta."
    )
  }

  pontos_sf |>
    dplyr::mutate(
      id_hex = h3jsr::point_to_cell(
        sf::st_geometry(pontos_sf),
        res = resolucao_hex
      )
    )
}

# FIM ==========================================================================
