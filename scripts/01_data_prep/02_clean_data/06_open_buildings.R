# ==============================================================================
# OPEN BUILDINGS — MORFOLOGIA URBANA (AGREGAÇÃO H3)
# Objetivo: importar as edificações do Google Open Buildings, filtrá-las para
# a área de estudo e calcular indicadores morfológicos por hexágono H3
# (número de edificações, área média e desvio-padrão da área construída),
# gerando variáveis preditoras para o modelo.
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
# 1. PARÂMETROS
# ==============================================================================

# caminho do grid hexagonal dasimétrico gerado no script de extração
arquivo_grid_dasimetrico <-
  here(pasta_saida, "grid_dasimetrico.rds")

# caminho do arquivo bruto do Google Open Buildings
arquivo_open_buildings <-
  here(pasta_entrada, "open_buildings", "94d_buildings.csv.gz")


# ==============================================================================
# 2. CARREGAR O GRID HEXAGONAL E DEFINIR A ÁREA DE FILTRAGEM
# ==============================================================================

grid_h3_sf <-
  readRDS(arquivo_grid_dasimetrico)
glimpse(grid_h3_sf)

# máscara do perímetro unificado da área de estudo, usada para filtrar
# espacialmente as edificações (mais preciso que apenas a bounding box)
mascara_regiao <-
  grid_h3_sf |>
  st_union() |>
  st_make_valid()

# bounding box em WGS84, usada como pré-filtro rápido antes do recorte
# espacial exato — descarta a maior parte dos registros sem precisar de
# operação geométrica ponto a ponto
bbox_regiao <-
  grid_h3_sf |>
  st_transform(crs = 4326) |>
  st_bbox()


# ==============================================================================
# 3. IMPORTAR E PRÉ-FILTRAR OS DADOS DO OPEN BUILDINGS
# ==============================================================================
# a base bruta do Open Buildings é muito grande (cobre toda a UF/país), por
# isso o filtro é feito em duas etapas: primeiro um recorte rápido por
# bounding box (retangular, barato), depois o recorte espacial exato pela
# máscara da área de estudo (seção 4)

# importando e pré-filtrando o Open Buildings por bounding box
open_buildings_bruto <-
  vroom(
    arquivo_open_buildings,
    col_select = c(
      longitude,
      latitude,
      area_in_meters,
      confidence
    ),
    show_col_types = FALSE
  ) |>
  rename(building_area = area_in_meters) |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    longitude >= bbox_regiao["xmin"],
    longitude <= bbox_regiao["xmax"],
    latitude >= bbox_regiao["ymin"],
    latitude <= bbox_regiao["ymax"]
  )

# converte para objeto espacial (pontos em WGS84)
open_buildings_sf <-
  open_buildings_bruto |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

rm(open_buildings_bruto)
gc()


# ==============================================================================
# 4. RECORTE ESPACIAL EXATO PARA A ÁREA DE ESTUDO
# ==============================================================================

# mantém apenas as edificações estritamente dentro do contorno da área de
# estudo (o pré-filtro por bounding box, sozinho, pode manter pontos nos
# "cantos" fora do polígono real)
open_buildings_regiao <-
  st_filter(open_buildings_sf, mascara_regiao)


# ==============================================================================
# 5. VINCULAR CADA EDIFICAÇÃO AO SEU HEXÁGONO H3
# ==============================================================================
# join espacial: casa cada ponto de edificação com o hexágono que o contém,
# em vez de recalcular o índice H3 ponto a ponto (mais rápido e garante que
# o resultado usa exatamente os mesmos id_hex do restante do pipeline)
open_buildings_h3 <-
  open_buildings_regiao |>
  st_join(
    select(grid_h3_sf, id_hex),
    join = st_intersects
  )


# ==============================================================================
# 6. AGREGAR OS INDICADORES MORFOLÓGICOS POR HEXÁGONO
# ==============================================================================

indicadores_por_hex <-
  open_buildings_h3 |>
  st_drop_geometry() |>
  filter(!is.na(id_hex)) |>
  group_by(id_hex) |>
  summarise(
    number_of_buildings = n(),
    avg_building_area = mean(building_area, na.rm = TRUE),
    # desvio-padrão da área construída — feature morfológica pedida
    # explicitamente na Tabela 1 do artigo ("standard deviation of the
    # building area")
    standard_deviation_of_building_area = sd(building_area, na.rm = TRUE),
    .groups = "drop"
  )

# parte do grid de referência completo para garantir que todos os
# hexágonos apareçam no resultado final — células sem nenhuma edificação
# recebem number_of_buildings = 0 e área/desvio-padrão como NA
open_buildings <-
  grid_h3_sf |>
  st_drop_geometry() |>
  select(id_hex) |>
  left_join(indicadores_por_hex, by = "id_hex") |>
  mutate(number_of_buildings = coalesce(number_of_buildings, 0L))


# ==============================================================================
# 7. VALIDAÇÃO E SALVAMENTO
# ==============================================================================

glimpse(open_buildings)
skimr::skim(open_buildings)

saveRDS(
  open_buildings,
  file.path(pasta_saida, "open_buildings.rds")
)

# FIM ==========================================================================
