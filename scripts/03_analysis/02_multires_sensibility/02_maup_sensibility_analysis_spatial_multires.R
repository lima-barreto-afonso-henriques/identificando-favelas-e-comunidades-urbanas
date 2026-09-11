# ==============================================================================
# EXTRAÇÃO DOS DADOS
# Objetivo: construir um grid hexagonal (H3) recortado pela área urbana do
# município e filtrado dasimetricamente pelos domicílios do CNEFE, mantendo
# apenas as células efetivamente habitadas.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================
# carrega funções e pacotes utilizados em todos os scripts do projeto
# install.packages("here")
# library(here)
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")

# ==============================================================================
# 0b. RESOLUÇÃO H3 A GERAR (parametrizado para a checagem de MAUP 9-10-11)
# ==============================================================================
# DIDÁTICA: no H3, quanto MAIOR o número da resolução, MENOR o hexágono
# (cada célula se divide em ~7 filhas a cada resolução que sobe).
#
# Rode este script INDIVIDUALMENTE para cada resolução que quiser gerar
# (mude 'resolucao_hex' abaixo e rode de novo) — não dá para automatizar
# num loop dentro de um único script, porque os outros 5 scripts de
# ingredientes (Censo, OSM, edificações, slope, favelas) dependem do
# 'grid_dasimetrico' desta etapa antes de poderem rodar; é preciso
# percorrer a cadeia inteira uma vez por resolução.
#
# Todas as saídas são SEMPRE sufixadas pela resolução (sem exceção,
# inclusive para a 10) — isso evita ambiguidade e mantém os nomes de
# arquivo simétricos entre as três resoluções testadas. O
# 'grid_dasimetrico.rds' original (sem sufixo), que o restante do
# pipeline já publicado depende, NUNCA é tocado por este script — os
# arquivos '_res<N>' são sempre novos, adicionais.
resolucao_hex <- 9 # <- mude aqui (9, 10 ou 11) e rode de novo a cada vez

nome_saida <- function(nome_base, extensao) {
  file.path(
    pasta_saida,
    paste0(nome_base, "_res", resolucao_hex, ".", extensao)
  )
}

message(sprintf(
  "Gerando grid dasimétrico na resolução H3 %d...",
  resolucao_hex
))


# ==============================================================================
# 1. PARÂMETROS E FILTROS GEOGRÁFICOS
# ==============================================================================

# código do município (ou lista de códigos, para regiões metropolitanas)
# ex.: 3550308 = São Paulo (SP)
codigo_municipio_filtro <- "3550308"

# baixa os setores censitários de todos os municípios da lista
# e une o resultado em um único objeto sf
setores_censitarios <-
  readRDS(file.path(pasta_saida, "setores_censitarios_ibge.rds"))
glimpse(setores_censitarios)

# reprojeta para o padrão geodésico do IBGE (SIRGAS 2000)
# e mantém apenas os setores de área urbana
municipio <-
  setores_censitarios |>
  st_transform(crs = 4674) |>
  # no pacote {geobr}, a coluna de situação chama-se 'zone'
  filter(zone == "Urbana" & code_muni %in% codigo_municipio_filtro)

# inspeção rápida do resultado
glimpse(municipio)
skimr::skim(municipio)


# ==============================================================================
# 2. CONTORNO DA ÁREA DE ESTUDO
# ==============================================================================

# corrige eventuais geometrias inválidas
# (auto-interseções, anéis abertos etc.)
# antes de qualquer operação topológica — isso resolve o problema na raiz,
# em vez de contorná-lo trocando de motor geométrico
municipio_valido <-
  st_make_valid(municipio)

# une os setores já com geometrias válidas
contorno_area <-
  st_union(municipio_valido)

# o H3 exige coordenadas em WGS84 (EPSG: 4326)
contorno_wgs84 <-
  st_transform(contorno_area, crs = 4326)


# ==============================================================================
# 3. GERAÇÃO DOS ÍNDICES HEXAGONAIS (H3)
# ==============================================================================

# (resolução já definida no topo do script, Seção 0b, como 'resolucao_hex')

# preenche o contorno da área de estudo com células H3
indices_hex <-
  contorno_wgs84 |>
  polygon_to_cells(res = resolucao_hex) |>
  unlist()


# ==============================================================================
# 4. CONVERSÃO DOS ÍNDICES EM POLÍGONOS (GRID)
# ==============================================================================

# converte cada índice H3 em sua geometria poligonal correspondente
geometrias_hex <-
  cell_to_polygon(indices_hex)

# monta o grid hexagonal como um objeto sf
grid_hexagonal <-
  st_as_sf(
    data.frame(
      id_hex = indices_hex,
      geometry = geometrias_hex
    )
  )

# plota apenas as geometrias
# (evita estouro de memória com muitos atributos)
plot(
  st_geometry(grid_hexagonal),
  main = "Grid Hexagonal H3"
)


# ==============================================================================
# 5. FILTRO DASIMÉTRICO (VIA CNEFE)
# ==============================================================================
# a ideia do filtro dasimétrico é manter apenas os hexágonos que contêm
# ao menos um domicílio, evitando representar como "habitadas" áreas
# vazias (parques, várzeas, terrenos não edificados etc.)

# carrega o CNEFE já tratado (endereços/domicílios geolocalizados)
cnefe_domicilios <-
  readRDS(file.path(pasta_saida, "ibge_cnefe.rds"))

# associa cada domicílio ao hexágono H3 em que ele está localizado
cnefe_domicilios <-
  cnefe_domicilios |>
  mutate(
    h3_id = h3jsr::point_to_cell(geometry, res = resolucao_hex)
  )

# conta o número de domicílios por hexágono
contagem_domicilios <-
  cnefe_domicilios |>
  st_drop_geometry() |>
  filter(!is.na(h3_id)) |>
  group_by(h3_id) |>
  summarise(num_residencias = n(), .groups = "drop")

# mantém no grid apenas os hexágonos com domicílios
# (inner_join descarta automaticamente as células sem correspondência na contagem)
grid_dasimetrico <-
  grid_hexagonal |>
  inner_join(
    contagem_domicilios,
    by = c("id_hex" = "h3_id")
  )

# plota o resultado final: grid filtrado apenas para as áreas habitadas
plot(
  st_geometry(grid_dasimetrico),
  main = "Grid Dasimétrico H3 (Apenas áreas habitadas)"
)


# ==============================================================================
# 6. SALVAR OS RESULTADOS
# ==============================================================================
# formato .rds: leitura rápida dentro do próprio fluxo em R
saveRDS(
  grid_dasimetrico,
  file = nome_saida("grid_dasimetrico", "rds")
)

# formato .gpkg: interoperável com QGIS e outros softwares de SIG
st_write(
  obj = grid_dasimetrico,
  dsn = nome_saida("grid_dasimetrico", "gpkg"),
  driver = "GPKG",
  delete_dsn = TRUE
)

message(sprintf(
  "Salvo: %s (%d hexágonos habitados, resolução %d)",
  nome_saida("grid_dasimetrico", "rds"),
  nrow(grid_dasimetrico),
  resolucao_hex
))

# FIM ==========================================================================
