# ==============================================================================
# EXTRAÇÃO DE DADOS BRUTOS (geobr, censobr, OSM, Open Buildings, Topodata)
# Objetivo: baixar TODAS as fontes de dados brutos usadas no pipeline e
# salvá-las em 'data/raw', na estrutura de pastas que os scripts seguintes
# (01 em diante) esperam encontrar. Este é o script 00 — roda antes de
# qualquer outro, e só precisa ser rodado de novo se os dados de origem
# mudarem ou se a máquina de trabalho for trocada.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS E LOCALIDADE
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# aumenta o tempo-limite de download do R (padrão é 60s) — arquivos como o
# do Open Buildings ou do Topodata podem levar mais de um minuto em
# conexões mais lentas, e o download seria interrompido no meio
options(timeout = 3600)

# caminho raiz onde todos os dados brutos deste script são salvos —
# mesmo nome (pasta_entrada) usado como ENTRADA em todos os scripts
# seguintes do pipeline
pasta_entrada <- here("data", "raw")

# VARIÁVEIS DE LOCALIDADE (Alterado de UF para Município onde possível)
# sigla da UF de interesse —
# mantida para consultas que ainda precisem do estado
# nome por extenso, como usado pelo censobr
nome_estado_filtro <- "São Paulo"

# Inserção das variáveis municipais para limitar a extração de dados
# Código IBGE de São Paulo
codigo_municipio_filtro <- 3550308
nome_municipio_filtro <- "São Paulo"


# ==============================================================================
# 1. FUNÇÃO AUXILIAR — GARANTIR QUE UMA PASTA EXISTE
# ==============================================================================
# pequena função de apoio para não repetir o mesmo bloco
# if (!dir.exists(...)) dir.create(...) em cada seção deste script

garantir_pasta <- function(caminho) {
  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE)
  }
  caminho
}


# ==============================================================================
# 2. SETORES CENSITÁRIOS (GEOBR)
# ==============================================================================
# baixa a malha de setores censitários do Censo de 2022)

pasta_geobr <-
  garantir_pasta(here(pasta_entrada, "geobr"))

# baixa a geometria só para o MUNICÍPIO de interesse
setores_censitarios <-
  geobr::read_census_tract(
    code_tract = codigo_municipio_filtro,
    year = 2022,
    showProgress = TRUE
  )
glimpse(setores_censitarios)

# save a base
saveRDS(
  setores_censitarios,
  here(pasta_geobr, "setores_censitarios_ibge.rds")
)

# remove a base que não será mais utilizada
rm(setores_censitarios)


# ==============================================================================
# 3. DADOS TABULARES DO CENSO — DOMICÍLIO E ENTORNO (CENSOBR)
# ==============================================================================

# baixa microdados do Censo de 2022
# Domicílio
# Entorno
pasta_censobr <-
  garantir_pasta(here(pasta_entrada, "censobr"))

# domicílios:
# perguntas sobre água, esgoto, resíduos, etc.
domicilios_municipio <-
  censobr::read_tracts(
    year = 2022,
    dataset = "Domicilio",
    as_data_frame = FALSE,
    showProgress = TRUE
  ) |>
  dplyr::filter(
    name_state == nome_estado_filtro,
    name_muni == nome_municipio_filtro
  ) |>
  dplyr::collect()
glimpse(domicilios_municipio)


saveRDS(domicilios_municipio, here(pasta_censobr, "domicilios_municipio.rds"))

# Infraestrutura do Entorno:
# perguntas sobre calçada, arborização, etc. (nível de quadra/setor)
entorno_municipio <-
  censobr::read_tracts(
    year = 2022,
    dataset = "Entorno",
    as_data_frame = FALSE,
    showProgress = TRUE
  ) |>
  dplyr::filter(
    name_state == nome_estado_filtro,
    name_muni == nome_municipio_filtro
  ) |>
  dplyr::collect()
glimpse(entorno_municipio)

# save a base
saveRDS(
  entorno_municipio,
  here(pasta_censobr, "entorno_municipio.rds")
)

# remove a base que não será mais utilizada
rm(domicilios_municipio, entorno_municipio)


# ==============================================================================
# 4. FAVELAS E COMUNIDADES URBANAS (FCUs) — SETORIZADAS E NÃO SETORIZADAS
# ==============================================================================
# "Setorizadas": polígonos já recortados pelos limites dos setores
# censitários (útil para cruzar diretamente com dados do Censo).
# "Não setorizadas": polígonos contínuos do assentamento, sem recorte por
# setor (útil para medir área total e formato da mancha urbana).
#
# Os dois arquivos vêm de URLs fixas do IBGE. Como o nome do arquivo "não
# setorizadas" inclui uma data de publicação no próprio nome
# (20260410), essa URL pode ficar desatualizada se o IBGE publicar uma
# nova versão — se o download falhar com erro 404, confira a data mais
# recente disponível em https://ftp.ibge.gov.br antes de qualquer outra
# depuração.
# OBS: O IBGE fornece esse dado apenas em escopo nacional/estadual fechado
# no zip, o recorte espacial municipal será feito nos scripts subsequentes.

# polígonos das Favelas e das Comunidades Urbanas (FCUs)
baixar_e_extrair_zip <- function(
  url,
  pasta_destino,
  nome_arquivo_zip,
  descricao
) {
  garantir_pasta(pasta_destino)
  caminho_zip <- here(pasta_destino, nome_arquivo_zip)

  if (file.exists(caminho_zip)) {
    message(sprintf("   -> %s já baixado — pulando etapa.", descricao))
    return(invisible(NULL))
  }

  download.file(url = url, destfile = caminho_zip, mode = "wb")
  unzip(caminho_zip, exdir = pasta_destino)
  message(sprintf("   -> %s baixado e extraído.", descricao))
}

# baixa os polígonos das Favelas e das Comunidades Urbanas (FCUs) setorizados
url_fcu_setorizadas <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/",
  "Favelas_e_comunidades_urbanas_Resultados_do_universo/",
  "arquivos_vetoriais/poligonos_FCUs_shp.zip"
)

baixar_e_extrair_zip(
  url_fcu_setorizadas,
  here(pasta_entrada, "favelas_comunidades_urbanas", "setorizadas"),
  "poligonos_FCUs_shp.zip",
  "FCUs setorizadas"
)


# baixa os polígonos das Favelas e das Comunidades Urbanas (FCUs) não setorizados
url_fcu_nao_setorizadas <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/",
  "Favelas_e_comunidades_urbanas_Resultados_do_universo/",
  "arquivos_vetoriais/FCUs_nao_setorizadas_shp_20260410.zip"
)

baixar_e_extrair_zip(
  url_fcu_nao_setorizadas,
  here(pasta_entrada, "favelas_comunidades_urbanas", "nao_setorizadas"),
  "FCUs_nao_setorizadas_shp_20260410.zip",
  "FCUs não setorizadas"
)


# ==============================================================================
# 5. OPEN BUILDINGS (GOOGLE)
# ==============================================================================

# baixa as edificações do Open Buildings da Google
pasta_open_buildings <-
  garantir_pasta(here(pasta_entrada, "open_buildings"))

url_open_buildings <- paste0(
  "https://storage.googleapis.com/",
  "open-buildings-data/v3/polygons_s2_level_4_gzip/",
  "94d_buildings.csv.gz"
)

# mantemos o arquivo compactado — vroom() lê .csv.gz nativamente (detecta
# a extensão e descompacta em memória, sob demanda), então descompactar em
# disco antes só gastaria tempo e espaço em dobro sem nenhum benefício.
# Os scripts 06 e 09 leem este mesmo arquivo '.csv.gz' diretamente.
arquivo_gz <- here(pasta_open_buildings, "94d_buildings.csv.gz")

if (!file.exists(arquivo_gz)) {
  download.file(url = url_open_buildings, destfile = arquivo_gz, mode = "wb")
  message("   -> Download do Open Buildings concluído.")
} else {
  message("   -> Arquivo do Open Buildings já existe — pulando download.")
}


# ==============================================================================
# 6. TOPODATA (INPE)
# ==============================================================================

# baixa dados de relevo do Topodata/INPE
pasta_topodata <-
  garantir_pasta(here(pasta_entrada, "topodata"))

# cada URL cobre uma folha (quadrícula) diferente do Topodata — a lista
# precisa incluir todas as folhas que tocam a área de estudo; se o
# município crescer para fora dessas folhas, adicione a URL correspondente
urls_topodata <- c(
  "http://www.dsr.inpe.br/topodata/data/geotiff/23S48_SN.zip",
  "http://www.dsr.inpe.br/topodata/data/geotiff/23S495SN.zip",
  "http://www.dsr.inpe.br/topodata/data/geotiff/24S495SN.zip",
  "http://www.dsr.inpe.br/topodata/data/geotiff/24S48_SN.zip",
  "http://www.dsr.inpe.br/topodata/data/geotiff/23S465SN.zip"
)

for (url in urls_topodata) {
  nome_arquivo <- basename(url)
  caminho_zip <- here(pasta_topodata, nome_arquivo)

  if (file.exists(caminho_zip)) {
    message(sprintf("   -> %s já existe — pulando.", nome_arquivo))
    next
  }

  message(sprintf("   -> Baixando %s...", nome_arquivo))
  download.file(url = url, destfile = caminho_zip, mode = "wb", quiet = TRUE)
  unzip(caminho_zip, exdir = pasta_topodata)
}

# ==============================================================================
# 7. OPENSTREETMAP (OSM) — INFRAESTRUTURA FÍSICA
# ==============================================================================
# Baixa aqui, uma única vez, os dados brutos de infraestrutura física do
# OSM e salva em disco.

# txtrai a infraestrutura física do OpenStreetMap

# evita timeout da API Overpass em consultas volumosas
options(osmdata.timeout = 600)

pasta_osm <- garantir_pasta(here(pasta_entrada, "osm"))

# bounding box da UF inteira, em WGS84 (padrão exigido pela API do OSM)
municipio_sf <-
  geobr::read_municipality(
    code_muni = codigo_municipio_filtro,
    year = 2022
  )

bbox_osm <-
  municipio_sf |>
  st_transform(crs = 4326) |>
  st_bbox()

# baixa uma categoria de feição do OSM, tratando com segurança o caso em
# que a categoria não existe na área consultada (retorna NULL em vez de
# quebrar o script)
baixar_osm <- function(bbox, key, value = NULL, tipo_geom = "osm_lines") {
  resultado <-
    opq(bbox = bbox) |>
    add_osm_feature(key = key, value = value) |>
    osmdata_sf()

  dados <- resultado[[tipo_geom]]
  if (is.null(dados) || nrow(dados) == 0) {
    return(NULL)
  }
  dados
}

# combina, com segurança, geometrias de tipos diferentes (ex.: linhas de
# pista + polígonos de aeródromo) em uma única camada
unificar_geometrias <- function(...) {
  camadas <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(camadas) == 0) {
    return(NULL)
  }
  do.call(c, lapply(camadas, st_geometry))
}

# Consulta a API Overpass
# (pode levar alguns minutos)

linhas_trem <- baixar_osm(
  bbox_osm,
  "railway",
  c("rail", "subway", "light_rail", "tram")
)
linhas_trem

rodovias <- baixar_osm(
  bbox_osm,
  "highway",
  c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified")
)
rodovias

linhas_energia <-
  baixar_osm(
    bbox_osm,
    "power",
    c("line", "minor_line")
  )
linhas_energia

rios <-
  baixar_osm(
    bbox_osm,
    "waterway",
    c("river", "stream", "canal", "drain", "ditch")
  )
rios

corpos_dagua <-
  baixar_osm(
    bbox_osm,
    "natural",
    "water",
    tipo_geom = "osm_polygons"
  )
corpos_dagua

aero_linhas <-
  baixar_osm(
    bbox_osm,
    "aeroway",
    tipo_geom = "osm_lines"
  )
aero_linhas

aero_poligono <-
  baixar_osm(
    bbox_osm,
    "aeroway",
    tipo_geom = "osm_polygons"
  )
aero_poligono


# verificar os dados
glimpse(linhas_trem)
glimpse(rodovias)
glimpse(linhas_energia)
glimpse(rios)
glimpse(corpos_dagua)
glimpse(aero_linhas)
glimpse(aero_poligono)

aeroportos_geom <-
  unificar_geometrias(aero_linhas, aero_poligono)

lista_osm <- list(
  linhas_trem = linhas_trem,
  rodovias = rodovias,
  linhas_energia = linhas_energia,
  rios = rios,
  corpos_dagua = corpos_dagua,
  aeroportos = aeroportos_geom
)

# resumo de diagnóstico: quais categorias vieram vazias (NULL) da consulta
for (nome_categoria in names(lista_osm)) {
  if (is.null(lista_osm[[nome_categoria]])) {
    message(sprintf(
      "   -> Aviso: nenhuma feição encontrada para '%s' na área consultada.",
      nome_categoria
    ))
  }
}

glimpse(lista_osm)

# salva a base de dados
saveRDS(lista_osm, here(pasta_osm, "open_street_maps.rds"))

# FIM ==========================================================================
