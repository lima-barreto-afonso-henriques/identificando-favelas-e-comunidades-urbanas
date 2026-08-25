# ==============================================================================
# CENSO 2022 — INDICADORES DE INFRAESTRUTURA (DOMICÍLIOS E ENTORNO)
# Objetivo: a partir dos microdados do Censo 2022 (pacote {censobr}), calcular
# por setor censitário as contagens de domicílios com acesso a rede de água,
# esgoto e coleta de resíduos, além dos indicadores de entorno (calçadas e
# arborização), e vincular essas contagens à malha espacial dos setores.
#
# Este script para na escala do SETOR CENSITÁRIO. A agregação para o grid
# hexagonal H3 (via filtro dasimétrico, mesma lógica do script de extração)
# é feita em um script posterior, que recebe este resultado como insumo.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")

# subpastas específicas deste script
pasta_censo_raw <- here(pasta_entrada, "censobr")
pasta_municipio <- here(pasta_entrada, "municipio")
pasta_malha_setores <- here(pasta_entrada, "geobr")

# ==============================================================================
# 1. PARÂMETROS
# ==============================================================================

# código do município (ou lista de códigos, para regiões metropolitanas)
# ex.: 3550308 = São Paulo (SP)
codigo_municipio_filtro <- "3550308"

# caminhos dos arquivos de entrada, já convertidos para .rds
arquivo_domicilios <- here(pasta_censo_raw, "domicilios_sp.rds")
arquivo_entorno <- here(pasta_censo_raw, "entorno_sp.rds")
arquivo_malha_setores <- here(
  pasta_malha_setores,
  "setores_censitarios_ibge.rds"
)


# ==============================================================================
# 2. IMPORTAR E FILTRAR OS MICRODADOS DO CENSO
# ==============================================================================

# leitura dos dados de domicílios e da infraestrutura do entorno
domicilios <-
  readRDS(arquivo_domicilios) |>
  filter(as.character(code_muni) %in% codigo_municipio_filtro)
glimpse(domicilios)

entorno <-
  readRDS(arquivo_entorno) |>
  filter(as.character(code_muni) %in% codigo_municipio_filtro)
glimpse(entorno)

# ==============================================================================
# 3. CONTAGENS DE INFRAESTRUTURA POR SETOR CENSITÁRIO
# ==============================================================================
# para cada tema, calculamos duas contagens por setor:
#   "_rede"  = domicílios atendidos pela forma de acesso mais adequada
#              (ex.: rede geral de água/esgoto, coleta direta de resíduos)
#   "_total" = total de domicílios respondentes ao quesito, somando todas
#              as categorias de resposta (adequadas ou não)
# essa razão "_rede / _total" é o que permite calcular, no script seguinte,
# o percentual de cobertura de cada infraestrutura por setor.

# Calculo da contagem de infraestrutura
# água: abastecimento por rede geral (V00119) sobre o total de respostas
contagem_agua <-
  domicilios |>
  group_by(code_tract) |>
  summarise(
    water_rede = sum(domicilio02_V00119, na.rm = TRUE),
    water_total = sum(
      coalesce(domicilio02_V00119, 0) +
        coalesce(domicilio02_V00120, 0) +
        coalesce(domicilio02_V00121, 0) +
        coalesce(domicilio02_V00122, 0) +
        coalesce(domicilio02_V00123, 0) +
        coalesce(domicilio02_V00124, 0) +
        coalesce(domicilio02_V00125, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
glimpse(contagem_agua)


# esgoto: rede geral ou pluvial (V00309 + V00310) sobre o total de respostas
contagem_esgoto <-
  domicilios |>
  group_by(code_tract) |>
  summarise(
    sewer_rede = sum(
      coalesce(domicilio02_V00309, 0) + coalesce(domicilio02_V00310, 0),
      na.rm = TRUE
    ),
    sewer_total = sum(
      coalesce(domicilio02_V00309, 0) +
        coalesce(domicilio02_V00310, 0) +
        coalesce(domicilio02_V00311, 0) +
        coalesce(domicilio02_V00312, 0) +
        coalesce(domicilio02_V00313, 0) +
        coalesce(domicilio02_V00314, 0) +
        coalesce(domicilio02_V00315, 0) +
        coalesce(domicilio02_V00316, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
glimpse(contagem_esgoto)

# resíduos: coleta direta ou indireta (V00397 + V00398) sobre o total
contagem_residuos <-
  domicilios |>
  group_by(code_tract) |>
  summarise(
    waste_rede = sum(
      coalesce(domicilio02_V00397, 0) + coalesce(domicilio02_V00398, 0),
      na.rm = TRUE
    ),
    waste_total = sum(
      coalesce(domicilio02_V00397, 0) +
        coalesce(domicilio02_V00398, 0) +
        coalesce(domicilio02_V00399, 0) +
        coalesce(domicilio02_V00400, 0) +
        coalesce(domicilio02_V00401, 0) +
        coalesce(domicilio02_V00402, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
glimpse(contagem_residuos)

# calçadas (indicador de entorno, não de domicílio)
contagem_calcadas <-
  entorno |>
  group_by(code_tract) |>
  summarise(
    sidewalk_rede = sum(coalesce(domicilios_V05021, 0), na.rm = TRUE),
    sidewalk_total = sum(
      coalesce(domicilios_V05021, 0) +
        coalesce(domicilios_V05022, 0) +
        coalesce(domicilios_V05023, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
glimpse(contagem_calcadas)

# arborização (indicador de entorno, não de domicílio)
contagem_arvores <-
  entorno |>
  group_by(code_tract) |>
  summarise(
    trees_rede = sum(
      coalesce(domicilios_V05031, 0) +
        coalesce(domicilios_V05032, 0) +
        coalesce(domicilios_V05033, 0),
      na.rm = TRUE
    ),
    trees_total = sum(
      coalesce(domicilios_V05030, 0) +
        coalesce(domicilios_V05031, 0) +
        coalesce(domicilios_V05032, 0) +
        coalesce(domicilios_V05033, 0) +
        coalesce(domicilios_V05034, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
glimpse(contagem_arvores)

# ==============================================================================
# 4. CONSOLIDAR AS CONTAGENS EM UMA ÚNICA TABELA
# ==============================================================================

censo_infraestrutura <-
  contagem_agua |>
  left_join(contagem_esgoto, by = "code_tract") |>
  left_join(contagem_residuos, by = "code_tract") |>
  left_join(contagem_calcadas, by = "code_tract") |>
  left_join(contagem_arvores, by = "code_tract") |>
  mutate(
    code_tract = as.character(code_tract),
    # setores sem nenhum domicílio respondente em um quesito ficam com NA
    # no left_join — tratamos como 0, já que não há infraestrutura a somar
    across(where(is.numeric), ~ coalesce(.x, 0)),
    code_tract = as.numeric(code_tract)
  )

glimpse(censo_infraestrutura)
skimr::skim(censo_infraestrutura)


# ==============================================================================
# 5. IMPORTAR E VALIDAR A MALHA ESPACIAL DOS SETORES CENSITÁRIOS
# ==============================================================================

malha_setores <-
  readRDS(arquivo_malha_setores) |>
  filter(
    as.character(code_muni) %in% codigo_municipio_filtro,
    zone == "Urbana"
  )
glimpse(malha_setores)

# checagem: geometrias inválidas antes da correção
# geometrias válidas antes da correção:
print(table(st_is_valid(malha_setores)))

# corrige eventuais geometrias inválidas
# (auto-interseções, anéis abertos etc.)
# antes de qualquer operação topológica
malha_setores <-
  malha_setores |>
  st_make_valid() |>
  st_transform(crs = 31983) |>
  # remove setores sem geometria
  filter(!st_is_empty(geometry)) |>
  # uniformiza o tipo de geometria
  st_cast("MULTIPOLYGON")

# geometrias válidas após a correção
print(table(st_is_valid(malha_setores)))


# ==============================================================================
# 6. VINCULAR AS CONTAGENS À MALHA ESPACIAL
# ==============================================================================

ibge_censo_setores_sf <-
  malha_setores |>
  inner_join(censo_infraestrutura, by = "code_tract")
glimpse(ibge_censo_setores_sf)

skimr::skim(st_drop_geometry(ibge_censo_setores_sf))


# ==============================================================================
# 7. SALVAR OS RESULTADOS
# ==============================================================================

saveRDS(
  ibge_censo_setores_sf,
  file.path(pasta_saida, "ibge_censo.rds")
)

# FIM ==========================================================================
