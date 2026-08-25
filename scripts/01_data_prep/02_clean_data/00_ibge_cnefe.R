# ==============================================================================
# PREPARAÇÃO DO CNEFE
# Objetivo: importar o Cadastro Nacional de Endereços para Fins Estatísticos
# (CNEFE), filtrar o município ou região metropolitana de estudo,
# recodificar as variáveis categóricas e converter os domicílios particulares
# em pontos espaciais (sf), servindo de insumo para o filtro dasimétrico
# do grid hexagonal.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw", "cnefe_ibge")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. PARÂMETROS
# ==============================================================================

# código do município (ou lista de códigos, para regiões metropolitanas)
# ex.: 3550308 = munícipio de São Paulo (SP)
codigo_municipio_filtro <- "3550308"

# caminho do arquivo bruto do CNEFE (um arquivo por UF)
arquivo_cnefe <- here(pasta_entrada, "35_SP.csv")


# ==============================================================================
# 2. IMPORTAÇÃO
# ==============================================================================

# inspeciona as primeiras linhas antes de importar a base inteira,
# como checagem rápida dos nomes e tipos de coluna
vroom(
  arquivo_cnefe,
  n_max = 10,
  show_col_types = FALSE
) |>
  glimpse()

# importa a base completa, já selecionando apenas as colunas necessárias
# (evita carregar na memória informações que não serão utilizadas)
cnefe_bruto <-
  vroom(
    arquivo_cnefe,
    col_select = c(
      COD_UF,
      COD_MUNICIPIO,
      LATITUDE,
      LONGITUDE,
      COD_ESPECIE,
      NV_GEO_COORD
    ),
    show_col_types = FALSE
  )
glimpse(cnefe_bruto)


# ==============================================================================
# 3. FILTRO DO MUNICÍPIO E PADRONIZAÇÃO DE NOMES
# ==============================================================================

# renomeia as colunas para nomes descritivos
# e mantém apenas os registros do município de estudo
ibge_cnefe <-
  cnefe_bruto |>
  rename(
    codigo_uf = COD_UF,
    codigo_municipio = COD_MUNICIPIO,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    codigo_especie_endereco = COD_ESPECIE,
    nivel_geocodificacao = NV_GEO_COORD
  ) |>
  filter(as.character(codigo_municipio) %in% codigo_municipio_filtro)
glimpse(ibge_cnefe)

# libera a memória ocupada pela base bruta, que não será mais utilizada.
rm(cnefe_bruto)
gc()


# ==============================================================================
# 4. RECODIFICAÇÃO DAS VARIÁVEIS CATEGÓRICAS
# ==============================================================================

# traduz os códigos numéricos do CNEFE para categorias legíveis
# e mantém apenas os domicílios particulares (exclui comércio, escolas,
# unidades de saúde, edificações em construção etc.)
ibge_cnefe_recodificado <-
  ibge_cnefe |>
  mutate(
    codigo_especie_endereco = case_match(
      as.character(codigo_especie_endereco),
      "1" ~ "domicilio particular",
      "2" ~ "domicilio coletivo",
      "3" ~ "agropecuario",
      "4" ~ "ensino",
      "5" ~ "saude",
      "6" ~ "outras finalidades",
      "7" ~ "edificacao em construcao",
      "8" ~ "religioso",
      .default = NA_character_
    ),
    nivel_geocodificacao = case_match(
      as.character(nivel_geocodificacao),
      "1" ~ "endereco",
      "2" ~ "endereco",
      "3" ~ "endereco",
      "4" ~ "face de quadra",
      "5" ~ "localidade",
      "6" ~ "setor censitario",
      .default = NA_character_
    )
  ) |>
  select(
    codigo_uf,
    codigo_municipio,
    codigo_especie_endereco,
    latitude,
    longitude,
    nivel_geocodificacao
  ) |>
  filter(codigo_especie_endereco == "domicilio particular")
glimpse(ibge_cnefe_recodificado)


# ==============================================================================
# 5. CONVERSÃO PARA OBJETO ESPACIAL (SF)
# ==============================================================================

# converte latitude/longitude para numérico, remove coordenadas ausentes,
# zeradas (0,0) ou fora do bounding box do Brasil, e então constrói a
# geometria de pontos em WGS84
ibge_cnefe_sf <-
  ibge_cnefe_recodificado |>
  mutate(
    latitude_num = suppressWarnings(as.numeric(latitude)),
    longitude_num = suppressWarnings(as.numeric(longitude))
  ) |>
  filter(
    !is.na(latitude_num),
    !is.na(longitude_num),
    latitude_num != 0,
    longitude_num != 0, # remove coordenadas (0,0)
    latitude_num >= -35,
    latitude_num <= 5, # bounding box do Brasil
    longitude_num >= -75,
    longitude_num <= -34
  ) |>
  st_as_sf(
    coords = c("longitude_num", "latitude_num"),
    # cria a geometria diretamente em WGS84
    crs = 4326,
    # remove as colunas de coordenadas, evitando duplicidade
    remove = TRUE
  ) |>
  # corrige eventuais geometrias inválidas na origem
  st_make_valid()

# ==============================================================================
# 6. CONTROLE DE QUALIDADE
# ==============================================================================
message("Geometrias vazias: ", sum(st_is_empty(ibge_cnefe_sf)))
glimpse(ibge_cnefe_sf)


# ==============================================================================
# 7. SALVAR OS RESULTADOS
# ==============================================================================

saveRDS(
  ibge_cnefe_sf,
  file.path(pasta_saida, "ibge_cnefe.rds")
)

# FIM ==========================================================================
