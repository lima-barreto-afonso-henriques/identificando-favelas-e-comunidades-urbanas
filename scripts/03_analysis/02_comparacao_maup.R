# ==============================================================================
# COMPARAÇÃO — MÉTODOS DE INTERPOLAÇÃO DASIMÉTRICA
# Objetivo: rodar as três opções de interpolação (área pura, dasimetria por
# edificações, picnofilático de Tobler) sobre a MESMA base (grid H3 +
# setores) e comparar de forma empírica — não só teórica — qual delas
# separa melhor hexágonos "Slum" de "Non_Slum".
#
# ESTRATÉGIA:
#   1. Roda os três scripts de interpolação (via source(), reaproveitando
#      as funções já definidas neles) ou carrega o resultado, se já salvo.
#   2. Junta os três datasets num único objeto, com colunas sufixadas por
#      método (_area, _edif, _pycno), para inspeção lado a lado.
#   3. Diagnóstico DESCRITIVO: correlação entre métodos por variável — um
#      sinal de quanto os métodos concordam ou discordam.
#   4. Diagnóstico ORIENTADO AO ALVO: para cada método, replica o teste t
#      entre hexágonos Slum e Non_Slum — o método que produzir a MAIOR
#      separação estatística (maior |estatística t|) é o que captura melhor
#      o contraste que o modelo final precisa aprender.
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

# código do município (ou lista de códigos, para regiões metropolitanas)
# ex.: 3550308 = São Paulo (SP)
codigo_municipio <- "3550308"

# variáveis sociais comuns às três interpolações, usadas na comparação
variaveis_sociais <- c(
  "households_connected_to_water_supply_network",
  "households_connected_to_sewerage_system",
  "households_solid_waste_collection",
  "households_with_sidewalk",
  "households_with_trees"
)


# ==============================================================================
# 2. EXECUTAR (OU CARREGAR, SE JÁ RODADO) AS TRÊS OPÇÕES
# ==============================================================================

arquivos_metodos <-
  list(
    area = file.path(pasta_saida, "ibge_social_area_pura.rds"),
    edif = file.path(pasta_saida, "ibge_social_dasimetrico_edificacoes.rds"),
    pycno = file.path(pasta_saida, "ibge_social_picnofilatico_tobler.rds")
  )

scripts_metodos <-
  list(
    area = here("scripts", "08_ibge_social_area_pura.R"),
    edif = here("scripts", "09_ibge_social_dasimetrico_edificacoes.R"),
    pycno = here("scripts", "10_ibge_social_picnofilatico_tobler.R")
  )

resultados_metodos <- list()

for (metodo in names(arquivos_metodos)) {
  caminho <- arquivos_metodos[[metodo]]

  if (!file.exists(caminho)) {
    message(
      ">>> Saída de '",
      metodo,
      "' não encontrada — rodando ",
      scripts_metodos[[metodo]],
      "..."
    )
    source(scripts_metodos[[metodo]])
  }

  resultados_metodos[[metodo]] <- readRDS(caminho)
}


# ==============================================================================
# 3. JUNTAR NUM ÚNICO OBJETO, COM SUFIXOS POR MÉTODO
# ==============================================================================

renomear_por_metodo <- function(dados, sufixo) {
  dados |> rename_with(~ paste0(.x, "_", sufixo), all_of(variaveis_sociais))
}

comparativo_metodos <-
  resultados_metodos$area |>
  renomear_por_metodo("area") |>
  inner_join(
    renomear_por_metodo(resultados_metodos$edif, "edif"),
    by = "id_hex"
  ) |>
  inner_join(
    renomear_por_metodo(resultados_metodos$pycno, "pycno"),
    by = "id_hex"
  )

glimpse(comparativo_metodos)

saveRDS(
  comparativo_metodos,
  file.path(pasta_saida, "ibge_social_comparativo_metodos.rds")
)


# ==============================================================================
# 4. DIAGNÓSTICO DESCRITIVO — CORRELAÇÃO ENTRE MÉTODOS POR VARIÁVEL
# ==============================================================================
# correlações muito altas (> 0.95) entre todos os métodos sugerem que a
# escolha do método pouco importa para essa variável (setores provavelmente
# homogêneos). Correlações mais baixas apontam para variáveis/regiões onde
# a escolha do método realmente muda o retrato — tipicamente onde a
# heterogeneidade intra-setor é maior

tabela_correlacao_metodos <-
  map_dfr(variaveis_sociais, function(variavel) {
    colunas <- paste0(variavel, c("_area", "_edif", "_pycno"))
    matriz <- comparativo_metodos |> select(all_of(colunas)) |> as.matrix()
    matriz_cor <- cor(matriz, use = "complete.obs", method = "spearman")

    tibble(
      variavel = variavel,
      cor_area_edif = matriz_cor[colunas[1], colunas[2]],
      cor_area_pycno = matriz_cor[colunas[1], colunas[3]],
      cor_edif_pycno = matriz_cor[colunas[2], colunas[3]]
    )
  })

print(tabela_correlacao_metodos)


# ==============================================================================
# 5. DIAGNÓSTICO ORIENTADO AO ALVO — QUAL MÉTODO SEPARA MELHOR SLUM/NON-SLUM?
# ==============================================================================
# reaproveita o rótulo já definido no script da variável alvo (is_slum),
# assumindo que ele já rodou e salvou 'favelas_e_comunidades_urbanas_area_pura.rds'

arquivo_rotulo <- file.path(
  pasta_saida,
  "favelas_e_comunidades_urbanas_area_pura.rds"
)

if (file.exists(arquivo_rotulo)) {
  rotulo <-
    readRDS(arquivo_rotulo) |>
    select(id_hex, is_slum) |>
    mutate(is_slum = if_else(is_slum == 1L, "Slum", "Non_Slum"))

  base_com_rotulo <- comparativo_metodos |> inner_join(rotulo, by = "id_hex")

  # teste t por método e variável: reproduz a comparação Slum vs. Non_Slum
  # separadamente para cada um dos três datasets de entrada
  colunas_por_metodo <-
    list(
      area = paste0(variaveis_sociais, "_area"),
      edif = paste0(variaveis_sociais, "_edif"),
      pycno = paste0(variaveis_sociais, "_pycno")
    )

  tabela_separacao <-
    map_dfr(names(colunas_por_metodo), function(metodo) {
      map_dfr(seq_along(variaveis_sociais), function(i) {
        coluna <- colunas_por_metodo[[metodo]][i]
        valores_slum <- base_com_rotulo |>
          filter(is_slum == "Slum") |>
          pull(!!coluna)
        valores_nao_slum <- base_com_rotulo |>
          filter(is_slum == "Non_Slum") |>
          pull(!!coluna)
        teste <- t.test(valores_slum, valores_nao_slum)

        tibble(
          metodo = metodo,
          variavel = variaveis_sociais[i],
          t_estatistica_abs = abs(teste$statistic),
          p_valor = teste$p.value
        )
      })
    })

  tabela_separacao_larga <-
    tabela_separacao |>
    pivot_wider(
      id_cols = variavel,
      names_from = metodo,
      values_from = t_estatistica_abs
    )

  print(tabela_separacao_larga)

  vencedor_por_variavel <-
    tabela_separacao |>
    group_by(variavel) |>
    slice_max(t_estatistica_abs, n = 1, with_ties = FALSE) |>
    ungroup() |>
    count(metodo, name = "n_variaveis_em_que_venceu") |>
    arrange(desc(n_variaveis_em_que_venceu))

  message(
    "\n>>> Método que mais vezes produziu maior separação estatística entre Slum/Non_Slum:"
  )
  print(vencedor_por_variavel)
} else {
  message(
    ">>> Artefato de rótulo (is_slum) não encontrado — pulando o diagnóstico ",
    "orientado ao alvo. Rode o script da variável alvo primeiro para ",
    "habilitar essa comparação."
  )
}


# ==============================================================================
# 6. SALVAR OS RESULTADOS
# ==============================================================================

message(
  "\n>>> Comparação concluída. 'ibge_social_comparativo_metodos.rds' contém ",
  "os três métodos lado a lado; use a tabela de separação estatística acima ",
  "para decidir qual método alimenta o modelo final — ou rode o modelo três ",
  "vezes, uma por método, para uma comparação definitiva no holdout espacial."
)

# FIM ==========================================================================
