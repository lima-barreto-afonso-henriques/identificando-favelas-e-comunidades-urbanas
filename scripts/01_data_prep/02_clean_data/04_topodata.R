# ==============================================================================
# TOPODATA — VARIÁVEIS TOPOGRÁFICAS (AGREGAÇÃO H3)
# Objetivo: importar os rasters de declividade do Topodata (INPE), recortá-los
# para a área de estudo e calcular a média de declividade dentro de cada
# hexágono do grid H3, gerando uma variável preditora para o modelo.
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
# sistema de coordenadas de destino: SIRGAS 2000 geográfico,
# padrão adotado pelos dados nacionais neste projeto
crs_destino <- "EPSG:4674"

# ==============================================================================
# 2. IMPORTAR O GRID HEXAGONAL PRÉ-PROCESSADO
# ==============================================================================

grid_h3_sf <- readRDS(file.path(pasta_saida, "grid_dasimetrico.rds"))
glimpse(grid_h3_sf)


# ==============================================================================
# 3. IMPORTAR OS ARQUIVOS RASTER DO TOPODATA
# ==============================================================================

# localiza todos os arquivos .tif dentro da pasta do Topodata
arquivos_tif <-
  list.files(
    path = here(pasta_entrada, "topodata"),
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )

# interrompe o script cedo se não houver nenhum raster a processar
if (length(arquivos_tif) == 0) {
  stop(
    "Nenhum arquivo .tif encontrado na pasta raw/topodata. 
    Verifique o diretório."
  )
}

# carrega cada arquivo .tif como um raster individual
message("Carregando os rasters do Topodata...")
lista_rasters <-
  lapply(arquivos_tif, rast)

# une os rasters em um único mosaico, caso a área de estudo abranja
# mais de uma folha do Topodata
message("Unificando os rasters em um único mosaico...")
if (length(lista_rasters) > 1) {
  topodata_bruto <- merge(sprc(lista_rasters))
} else {
  topodata_bruto <- lista_rasters[[1]]
}

# corrige o CRS declarado nos metadados originais do INPE, que costuma
# vir desatualizado/instável nos arquivos do Topodata
crs(topodata_bruto) <- "EPSG:4618"


# ==============================================================================
# 4. REPROJEÇÃO PARA UM CRS COMUM
# ==============================================================================

# raster e grid precisam estar no mesmo sistema de coordenadas antes de
# qualquer operação espacial entre eles
topodata_reprojetado <-
  terra::project(topodata_bruto, crs_destino)

grid_h3_sf <-
  st_transform(grid_h3_sf, crs = crs_destino)


# ==============================================================================
# 5. RECORTE DO RASTER PARA A ÁREA DE ESTUDO
# ==============================================================================

# converte o grid para o formato vetorial exigido pelo {terra}
grid_h3_terra <-
  vect(grid_h3_sf)

# recorta o raster aos limites do grid, reduzindo drasticamente o uso
# de memória nas etapas seguintes
topodata_recortado <-
  crop(
    topodata_reprojetado,
    grid_h3_terra,
    mask = TRUE
  )

# checagem visual: o recorte deve cobrir exatamente a área de estudo
plot(
  topodata_recortado,
  main = "Raster Topodata Recortado"
)


# ==============================================================================
# 6. ESTATÍSTICA ZONAL — MÉDIA DE DECLIVIDADE POR HEXÁGONO
# ==============================================================================

# calcula a média dos pixels do raster dentro de cada hexágono
extracao_topodata <-
  terra::extract(
    topodata_recortado,
    grid_h3_terra,
    fun = mean,
    na.rm = TRUE
  )

# o terra::extract retorna um data.frame em que a coluna 1 é o ID
# posicional do hexágono e a coluna 2 é o valor médio do pixel —
# por isso indexamos explicitamente a coluna 2
grid_h3_resultados <-
  grid_h3_sf |>
  mutate(avg_slope_topodata = extracao_topodata[, 2])


# ==============================================================================
# 7. VALIDAÇÃO
# ==============================================================================

glimpse(grid_h3_resultados)
summary(grid_h3_resultados)

# checagem visual rápida da agregação por hexágono
plot(
  grid_h3_resultados["avg_slope_topodata"],
  main = "Média Topodata por Hexágono H3"
)


# ==============================================================================
# 8. SALVAR OS RESULTADOS
# ==============================================================================

# monta o dataset tabular limpo (sem geometria), pronto para ser
# incorporado ao modelo preditivo
topodata_final <-
  grid_h3_resultados |>
  st_drop_geometry() |>
  select(id_hex, avg_slope_topodata)

saveRDS(
  topodata_final,
  file.path(pasta_saida, "topodata_declividade.rds")
)

# FIM ==========================================================================
