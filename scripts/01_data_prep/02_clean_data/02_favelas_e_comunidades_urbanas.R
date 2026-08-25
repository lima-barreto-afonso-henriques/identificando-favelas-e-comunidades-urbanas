# ==============================================================================
# VARIÁVEL ALVO (TARGET Y) — is_slum / slum_coverage_pct
# Objetivo: construir o RÓTULO que o modelo aprende a prever — se um
# hexágono é ou não classificado como favela/comunidade urbana — a partir
# dos polígonos oficiais do IBGE (2022). Corresponde à Seção 3.3 do artigo.
#
# O script calcula DUAS definições de rótulo, lado a lado, para permitir
# comparação:
#
#   Definição 1 — ÁREA PURA (a mesma do artigo original):
#     um hexágono é "Slum" quando o polígono oficial de favela cobre pelo
#     menos 70% da sua ÁREA GEOMÉTRICA. Limitação: essa área pode incluir
#     terreno vazio, vegetação ou vias largas dentro do perímetro do
#     polígono, superestimando a ocupação real.
#
#   Definição 2 — PONDERADA POR EDIFICAÇÃO:
#     em vez de medir área, mede-se a fração da ÁREA CONSTRUÍDA real
#     (edificações do Open Buildings) que está dentro do polígono de
#     favela. Corrige tanto hexágonos com alta cobertura de área mas
#     pouca ocupação real, quanto hexágonos com baixa cobertura de área
#     mas alta concentração de edificações dentro do polígono.
#
# Não aplicamos aqui a suavização de Tobler (usada em outros scripts do
# projeto): esse método redistribui uma contagem/massa de um polígono de
# origem preservando o total, e os polígonos de favela não carregam
# nenhuma contagem associada — são apenas a forma do assentamento.
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

# limiar de classificação: a partir de qual % de cobertura o hexágono
# é considerado "Slum" — mesmo limiar usado no artigo (Seção 3.3)
limiar_classificacao <- 0.70

# caminho do grid hexagonal dasimétrico gerado no script de extração
arquivo_grid_dasimetrico <-
  here(pasta_saida, "grid_dasimetrico.rds")

# caminho dos polígonos oficiais de favelas e comunidades urbanas (IBGE 2022)
arquivo_favelas <-
  here(
    pasta_entrada,
    "favelas_comunidades_urbanas",
    "setorizadas",
    "qg_2022_670_fcu_agreg.shp"
  )

# caminho da base bruta do Open Buildings (usada na Definição 2)
arquivo_open_buildings <-
  here(pasta_entrada, "open_buildings", "94d_buildings.csv")


# ==============================================================================
# 2. CARREGAR E PREPARAR AS CAMADAS ESPACIAIS
# ==============================================================================

# carrega o grid hexagonal dasimétrico (já filtrado para áreas habitadas)
grid_dasimetrico <-
  readRDS(arquivo_grid_dasimetrico)
glimpse(grid_dasimetrico)

# carrega os polígonos oficiais de favelas, filtrando o município de estudo
favelas_sf <-
  read_sf(arquivo_favelas, options = "ENCODING=UTF-8") |>
  filter(as.character(cd_mun) %in% codigo_municipio) |>
  st_make_valid()

# reprojeta o grid para SIRGAS 2000 / UTM 23S (metros), necessário para
# calcular áreas em m², e calcula a área total de cada hexágono
grid_proj <-
  grid_dasimetrico |>
  st_transform(crs = 31983) |>
  st_make_valid() |>
  mutate(hex_area = st_area(geometry))

# reprojeta os polígonos de favela para o mesmo sistema de coordenadas
favelas_proj <-
  favelas_sf |>
  st_transform(crs = 31983) |>
  st_make_valid()


# ==============================================================================
# 3. DEFINIÇÃO 1 — COBERTURA POR ÁREA PURA
# ==============================================================================

# passo 1: interseção geométrica entre cada hexágono e os polígonos de favela
intersecao_hex_favelas <-
  st_intersection(grid_proj, favelas_proj) |>
  mutate(area_intersecao = st_area(geometry))

# passo 2: soma a área de favela dentro de cada hexágono
# (um hexágono pode intersectar mais de um polígono de favela)
area_favela_por_hex <-
  intersecao_hex_favelas |>
  st_drop_geometry() |>
  group_by(id_hex) |>
  summarise(
    area_favela_total = sum(area_intersecao, na.rm = TRUE),
    .groups = "drop"
  )

# passo 3: calcula o percentual de cobertura e classifica o hexágono
# hexágonos sem nenhuma interseção recebem cobertura = 0 (Non_Slum)
target_area_pura <-
  grid_proj |>
  st_drop_geometry() |>
  left_join(area_favela_por_hex, by = "id_hex") |>
  mutate(
    area_favela_total = coalesce(area_favela_total, units::set_units(0, "m^2")),
    slum_coverage_pct = as.numeric(area_favela_total / hex_area),
    is_slum = if_else(slum_coverage_pct >= limiar_classificacao, 1L, 0L)
  ) |>
  select(id_hex, slum_coverage_pct, is_slum)

# checagem: distribuição de hexágonos classificados como favela
message("\n[Definição 1 - Área pura] Distribuição da variável alvo:")
print(table(target_area_pura$is_slum))
print(round(prop.table(table(target_area_pura$is_slum)) * 100, 2))


# ==============================================================================
# 4. DEFINIÇÃO 2 — COBERTURA PONDERADA POR EDIFICAÇÃO
# ==============================================================================
# em vez de medir área geométrica, medimos a fração da área CONSTRUÍDA
# real (edificações do Open Buildings) que está dentro do polígono de
# favela. Isso evita contar como "ocupado" um terreno vazio ou uma via
# larga que apenas geometricamente cai dentro do polígono.

if (!file.exists(arquivo_open_buildings)) {
  # se a base de edificações não estiver disponível, avisa e segue apenas
  # com a Definição 1 — o script não é interrompido
  warning(
    "Arquivo do Open Buildings não encontrado em ",
    arquivo_open_buildings,
    " — pulando a Definição 2 (ponderada por edificação)."
  )
  target_edificacoes <- NULL
} else {
  # passo 1: importa as edificações e converte para pontos espaciais
  edificacoes_sf <-
    vroom(
      arquivo_open_buildings,
      col_select = c(longitude, latitude, area_in_meters, confidence),
      show_col_types = FALSE
    ) |>
    rename(area_edificacao = area_in_meters) |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(crs = 31983)

  # passo 2: associa cada edificação ao hexágono em que ela está localizada
  message("Vinculando edificações ao hexágono correspondente...")
  edificacoes_com_hex <-
    edificacoes_sf |>
    st_join(select(grid_proj, id_hex), join = st_within) |>
    filter(!is.na(id_hex))

  # passo 3: identifica quais edificações caem dentro de um polígono de favela
  message("Identificando edificações dentro dos polígonos de favela...")
  edificacoes_com_hex <-
    edificacoes_com_hex |>
    mutate(
      dentro_favela = lengths(st_intersects(
        edificacoes_com_hex,
        favelas_proj
      )) >
        0
    )

  # passo 4: agrega por hexágono — área construída total vs. área construída
  # dentro de favela (ponderação por área, não por contagem de edificações,
  # para dar mais peso a construções maiores)
  cobertura_edificacao_por_hex <-
    edificacoes_com_hex |>
    st_drop_geometry() |>
    group_by(id_hex) |>
    summarise(
      n_edificacoes_total = n(),
      n_edificacoes_em_favela = sum(dentro_favela),
      area_construida_total = sum(area_edificacao, na.rm = TRUE),
      area_construida_em_favela = sum(
        area_edificacao[dentro_favela],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      slum_coverage_pct_edif = if_else(
        area_construida_total > 0,
        area_construida_em_favela / area_construida_total,
        0
      )
    )

  # passo 5: hexágonos sem nenhuma edificação mapeada ficam com cobertura
  # zero — coerente com o filtro dasimétrico já aplicado ao grid
  n_hex_sem_edificacao <-
    grid_proj |>
    st_drop_geometry() |>
    anti_join(cobertura_edificacao_por_hex, by = "id_hex") |>
    nrow()

  if (n_hex_sem_edificacao > 0) {
    message(
      n_hex_sem_edificacao,
      " hexágonos sem nenhuma edificação mapeada — tratados como ",
      "Non_Slum (slum_coverage_pct_edif = 0)."
    )
  }

  # passo 6: junta ao grid completo e classifica
  target_edificacoes <-
    grid_proj |>
    st_drop_geometry() |>
    select(id_hex) |>
    left_join(cobertura_edificacao_por_hex, by = "id_hex") |>
    mutate(
      slum_coverage_pct_edif = coalesce(slum_coverage_pct_edif, 0),
      is_slum_edif = if_else(
        slum_coverage_pct_edif >= limiar_classificacao,
        1L,
        0L
      )
    ) |>
    select(id_hex, slum_coverage_pct_edif, is_slum_edif)

  # checagem: distribuição de hexágonos classificados como favela
  message(
    "\n[Definição 2 - Ponderada por edificação] Distribuição da variável alvo:"
  )
  print(table(target_edificacoes$is_slum_edif))
  print(round(prop.table(table(target_edificacoes$is_slum_edif)) * 100, 2))
}


# ==============================================================================
# 5. COMPARAÇÃO ENTRE AS DUAS DEFINIÇÕES
# ==============================================================================
# quantos hexágonos mudam de classe (Slum <-> Non_Slum) dependendo do
# critério usado — útil para análise de sensibilidade do modelo final

if (!is.null(target_edificacoes)) {
  comparativo_definicoes <-
    target_area_pura |>
    inner_join(target_edificacoes, by = "id_hex") |>
    mutate(
      concordancia = is_slum == is_slum_edif,
      diferenca_pct_pontos = (slum_coverage_pct_edif - slum_coverage_pct) * 100
    )

  n_discordantes <- sum(!comparativo_definicoes$concordancia)
  pct_discordantes <- 100 * n_discordantes / nrow(comparativo_definicoes)

  message(sprintf(
    "\n>>> %d de %d hexágonos (%.1f%%) mudam de classe dependendo do ",
    n_discordantes,
    nrow(comparativo_definicoes),
    pct_discordantes
  ))
  message("    critério usado (área pura vs. ponderado por edificação).")
  message(
    ">>> Recomenda-se inspecionar os hexágonos discordantes no Google ",
    "Street View (mesma prática de avaliação qualitativa do artigo, ",
    "Seção 3.6) para decidir qual definição é mais fiel à realidade."
  )

  saveRDS(
    comparativo_definicoes,
    file.path(pasta_saida, "favelas_comunidades_comparativo_definicoes.rds")
  )
}


# ==============================================================================
# 6. SALVAR OS RESULTADOS
# ==============================================================================
glimpse(target_area_pura)

# definição 1: reproduz fielmente o critério do artigo original
saveRDS(
  target_area_pura,
  file.path(pasta_saida, "favelas_e_comunidades_urbanas_area_pura.rds")
)

# definição 2: critério alternativo, ponderado por edificação
if (!is.null(target_edificacoes)) {
  saveRDS(
    target_edificacoes,
    file.path(
      pasta_saida,
      "favelas_e_comunidades_urbanas_ponderado_edificacoes.rds"
    )
  )
}

message(
  "\n>>> Rótulos salvos. Use 'favelas_e_comunidades_urbanas_area_pura.rds' ",
  "para reproduzir o artigo fielmente, ou a versão '_ponderado_edificacoes' ",
  "para testar a definição alternativa de rótulo."
)

# FIM ==========================================================================
