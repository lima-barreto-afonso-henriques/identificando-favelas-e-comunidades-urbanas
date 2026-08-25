# ==============================================================================
# OPENSTREETMAP — VULNERABILIDADE FÍSICA (AGREGAÇÃO H3)
# Objetivo: identificar hexágonos localizados próximos a elementos de risco
# físico mapeados no OpenStreetMap (ferrovias, rodovias, linhas de energia,
# cursos d'água, corpos d'água e aeroportos), criando indicadores binários
# de proximidade (buffer de 100 m) para uso como variáveis preditoras.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw", "osm")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. PARÂMETROS
# ==============================================================================

# distância do buffer de vulnerabilidade física, em metros
distancia_buffer <- 100

# CRS métrico (SIRGAS 2000 / UTM 23S) usado para calcular buffers em metros
crs_metrico <- 31983

# caminho do grid hexagonal dasimétrico gerado no script de extração
arquivo_grid_dasimetrico <- here(pasta_saida, "grid_dasimetrico.rds")

# caminho do OpenStreetMaps gerado no script de extração
arquivo_osm <- here(pasta_entrada, "open_street_maps.rds")

# ==============================================================================
# 2. CARREGAR O GRID HEXAGONAL PRÉ-PROCESSADO
# ==============================================================================

grid_h3_sf <-
  readRDS(arquivo_grid_dasimetrico)
glimpse(grid_h3_sf)


# ==============================================================================
# 3. CONSULTAR OS ELEMENTOS FÍSICOS NO OPENSTREETMAP
# ==============================================================================
# cada consulta busca uma categoria de elemento dentro da bounding box da
# área de estudo e extrai apenas a geometria (osmdata_sf retorna vários
# tipos de geometria — usamos a que corresponde a cada elemento)

dados_osm <-
  readRDS(arquivo_osm)
glimpse(dados_osm)


# ==============================================================================
# 4. CRIAÇÃO DOS BUFFERS DE VULNERABILIDADE (100 M)
# ==============================================================================
# função auxiliar: cria um buffer de 100 m ao redor de um elemento OSM,
# já unindo as geometrias sobrepostas em uma única feição por categoria.
# Trata o caso em que a API não retorna nenhuma feição para a categoria
# (ex.: município sem aeroporto mapeado), evitando que o script quebre —
# nesse caso, devolve uma geometria vazia com o CRS correto, o que faz
# com que st_intersects simplesmente não encontre nenhuma interseção.
criar_buffer_vulnerabilidade <- function(dados_osm) {
  dados_vazios <-
    is.null(dados_osm) ||
    (inherits(dados_osm, "sf") && nrow(dados_osm) == 0) ||
    (inherits(dados_osm, "sfc") && length(dados_osm) == 0)

  if (dados_vazios) {
    return(st_sfc(st_point(), crs = crs_metrico))
  }

  dados_osm |>
    st_transform(crs = crs_metrico) |>
    st_make_valid() |>
    st_buffer(dist = distancia_buffer) |>
    st_union() |>
    st_make_valid()
}

# Calculando as zonas de amortecimento com buffers de 100 metros
buffer_ferrovia <- criar_buffer_vulnerabilidade(linhas_trem)
buffer_rodovia <- criar_buffer_vulnerabilidade(rodovias)
buffer_energia <- criar_buffer_vulnerabilidade(linhas_energia)
buffer_agua <- criar_buffer_vulnerabilidade(rios)
buffer_natural <- criar_buffer_vulnerabilidade(corpos_dagua)
buffer_aeroporto <- criar_buffer_vulnerabilidade(aeroportos_geom)


# ==============================================================================
# 5. REPROJEÇÃO DO GRID PARA O CRS MÉTRICO
# ==============================================================================
# usamos o mesmo grid carregado na seção 2, apenas reprojetado — não
# recriamos os hexágonos com polygon_to_cells, para preservar exatamente
# os mesmos id_hex do restante do pipeline
grid_h3_metrico <-
  grid_h3_sf |>
  st_transform(crs = crs_metrico)


# ==============================================================================
# 6. EXTRAÇÃO DOS INDICADORES DE INTERSEÇÃO (S2 ATIVO)
# ==============================================================================
# lengths(st_intersects(...)) > 0 identifica, para cada hexágono, se ele
# toca algum dos buffers de vulnerabilidade — técnica estável mesmo quando
# o buffer é uma geometria vazia (retorna comprimento 0, ou seja, FALSE).
# O motor S2 permanece ativo durante toda a operação; as geometrias já
# foram corrigidas com st_make_valid() na criação dos buffers.

# executa a análise de interseção espacial por hexágono
grid_osm_indicadores <-
  grid_h3_metrico |>
  mutate(
    int_railway = lengths(st_intersects(geometry, buffer_ferrovia)) > 0,
    int_highway = lengths(st_intersects(geometry, buffer_rodovia)) > 0,
    int_power = lengths(st_intersects(geometry, buffer_energia)) > 0,
    int_waterway = lengths(st_intersects(geometry, buffer_agua)) > 0,
    int_natural = lengths(st_intersects(geometry, buffer_natural)) > 0,
    int_aeroway = lengths(st_intersects(geometry, buffer_aeroporto)) > 0
  )
glimpse(grid_osm_indicadores)

# ==============================================================================
# 7. PADRONIZAR NOMES E SALVAR OS RESULTADOS
# ==============================================================================

osm_indicadores <-
  grid_osm_indicadores |>
  st_drop_geometry() |>
  rename(
    railway_intersection = int_railway,
    highway_intersection = int_highway,
    power_intersection = int_power,
    waterway_intersection = int_waterway,
    natural_intersection = int_natural,
    aeroway_intersection = int_aeroway
  ) |>
  select(
    id_hex,
    railway_intersection,
    highway_intersection,
    power_intersection,
    waterway_intersection,
    natural_intersection,
    aeroway_intersection
  )
glimpse(osm_indicadores)

saveRDS(
  osm_indicadores,
  file.path(pasta_saida, "open_street_maps.rds")
)

# FIM ==========================================================================
