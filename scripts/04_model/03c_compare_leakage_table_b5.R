# ==============================================================================
# DEMONSTRAÇÃO DE VAZAMENTO DE DADOS (DATA LEAKAGE) NA TABELA B.5
# (comparação dos 6 algoritmos, Seção 5 do 01_model_aricle.R / Apêndice B)
# ==============================================================================
# OBJETIVO DESTE SCRIPT:
# Mesma lógica de 03a_compare_leakage_article_sensibility_analysis.R e
# 03b_..._v2.R (SMOTENC-antes-do-split para a Tabela A.4), agora aplicada à
# ESCOLHA DO MODELO (Tabela B.5) em vez do limiar de cobertura.
#
# A frase do artigo (Seção 3.5) que descreve esse procedimento é do MESMO
# tipo ambíguo que já identificamos no Apêndice A:
#   "During cross-validation, the entire dataset was resampled over 10
#    iterations, using 90% for training and 10% for validation."
# — de novo não deixa claro se o balanceamento (SMOTENC) veio antes ou
# depois desse resample em 10 folds. Como é praticamente o mesmo texto
# (mesma estrutura de frase, mesmo tipo de ambiguidade) que já rendeu
# evidência de vazamento na Tabela A.4, é razoável testar se o MESMO
# padrão de implementação (e o mesmo tipo de erro) foi reaproveitado aqui.
#
# POR QUE ISSO IMPORTA PARA A ESCOLHA DO MODELO:
# Se a Tabela B.5 também sofreu leakage, o Random Forest não teria sido
# necessariamente escolhido por ser o melhor algoritmo — teria sido
# escolhido por ser o que melhor "aproveitou" a inflação artificial de
# métricas (ou simplesmente porque QUALQUER modelo treinado nesse regime
# acerta demais). Um sinal específico de alerta nos seus próprios dados:
# o KNN da Tabela B.5 (Fig./Tabela B.5 do artigo) tem recall = 0.96 — um
# valor MUITO alto para KNN nesse tipo de problema. KNN é especialmente
# sensível a vazamento por proximidade: se um ponto sintético (quase-
# duplicado de um ponto real) cai na validação enquanto o "gerador" dele
# está no treino, o KNN só precisa achar o vizinho mais próximo (ele
# mesmo, quase) para acertar — um sintoma mais direto de leakage do que
# em modelos baseados em árvore.
#
# COMO USAR:
# Rode este script DEPOIS do 01_model_aricle.R (mesma 'resolucao_hex') —
# ele reaproveita 'tabela_b5_reconstruida' do artefato salvo por aquele
# script como a versão HONESTA de comparação, e roda uma segunda versão
# COM leakage (SMOTENC antes do split) sobre os mesmos 6 algoritmos, para
# comparação lado a lado com a Tabela B.5 do artigo.
# ==============================================================================

# ==============================================================================
# 0. PACOTES E CAMINHOS
# ==============================================================================
gc()

source(here::here("scripts", "00_functions", "00_setup.R"))

pasta_entrada <- here("data", "processed")
pasta_saida <- here("outputs", "artefato")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# Mesmo padrão de resolução do resto do pipeline: rode individualmente
# para 9, 10 ou 11.
resolucao_hex <- 10 # <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
sufixo_saida <- paste0("_res", resolucao_hex)

# ==============================================================================
# 1. IMPORTAÇÃO — MESMOS DADOS E LIMIAR DO 01_model_aricle.R
# ==============================================================================
caminho_artefato_modelo <- file.path(
  pasta_saida,
  paste0("modelo_artigo", sufixo_saida, ".rds")
)

if (!file.exists(caminho_artefato_modelo)) {
  stop(
    "Artefato do modelo não encontrado em ",
    caminho_artefato_modelo,
    ". Rode 01_model_aricle.R primeiro (com a mesma 'resolucao_hex' ",
    "definida aqui) — este script reaproveita a Tabela B.5 honesta salva ",
    "lá para a comparação final."
  )
}

artefato_modelo <- readRDS(caminho_artefato_modelo)
tabela_b5_honesta <- artefato_modelo$tabela_b5_reconstruida
limiar_fracao <- artefato_modelo$limiar_decisao_artigo # o mesmo 70%/limiar_id herdado da sensibilidade

message(sprintf(
  ">>> Limiar de cobertura herdado (mesmo do 01_model_aricle.R): %.0f%%",
  limiar_fracao * 100
))

arquivos_entrada <- c(
  "9" = "grid_hexagonal_area_pura_res9.rds",
  "10" = "grid_hexagonal_area_pura_res10.rds",
  "11" = "grid_hexagonal_area_pura_res11.rds"
)
arquivo_entrada <- arquivos_entrada[[as.character(resolucao_hex)]]

grid_hexagonal <- readRDS(file.path(pasta_entrada, arquivo_entrada))

dados_base <-
  grid_hexagonal |>
  st_transform(31983) |>
  select(
    id_hex,
    slum_coverage_pct,
    number_private_residences,
    households_connected_to_water_supply_network,
    households_connected_to_sewerage_system,
    households_solid_waste_collection,
    households_with_sidewalk,
    households_with_trees,
    railway_intersection,
    highway_intersection,
    power_intersection,
    waterway_intersection,
    natural_intersection,
    aeroway_intersection,
    number_of_buildings,
    avg_building_area,
    standard_deviation_of_building_area,
    avg_slope_topodata
  ) |>
  st_drop_geometry() |>
  mutate(across(where(is.logical), as.numeric)) |>
  mutate(
    is_slum = factor(
      if_else(slum_coverage_pct >= limiar_fracao, "Slum", "Non_Slum"),
      levels = c("Slum", "Non_Slum")
    )
  ) |>
  select(-slum_coverage_pct)

message(sprintf(
  ">>> %d hexágonos carregados | prevalência real de 'Slum': %.2f%%",
  nrow(dados_base),
  100 * mean(dados_base$is_slum == "Slum")
))

# ==============================================================================
# 2. PARALELIZAÇÃO — SEMPRE SEQUENCIAL (mesmo motivo do 03a/03b)
# ==============================================================================
# DIDÁTICA: aqui também o SMOTENC é aplicado FORA da recipe, sobre o
# dataset inteiro, de propósito — o mesmo padrão que já estourou o limite
# de globals do 'future' (953,67 MiB) no script 03a. Sequencial evita
# repetir esse erro.
future::plan(sequential)

# ==============================================================================
# 3. MESMOS 6 ALGORITMOS DA TABELA B.5 (idêntico ao 01_model_aricle.R)
# ==============================================================================
especificacoes_modelos <-
  list(
    decision_tree = decision_tree() |>
      set_engine("rpart") |>
      set_mode("classification"),

    random_forest = rand_forest(trees = 500) |>
      set_engine("ranger") |>
      set_mode("classification"),

    gradient_boosting = boost_tree(trees = 500) |>
      set_engine("lightgbm") |>
      set_mode("classification"),

    adaboost = boost_tree(trees = 50) |>
      set_engine("C5.0") |>
      set_mode("classification"),

    xgboost = boost_tree(trees = 500) |>
      set_engine("xgboost") |>
      set_mode("classification"),

    knn = nearest_neighbor(neighbors = 5) |>
      set_engine("kknn") |>
      set_mode("classification")
  )

# ==============================================================================
# 4. VERSÃO "COM LEAKAGE" — SMOTENC ANTES DO SPLIT, MESMOS 6 ALGORITMOS
# ==============================================================================
# DIDÁTICA — mesma marcação de linhas sintéticas do 03b (Seção 4a/4b), para
# também testar aqui se a fração de 'Slum' sintético nos folds de
# validação é alta o bastante para explicar métricas infladas — só que
# agora é UM único número (não por limiar, já que a Tabela B.5 usa um
# único limiar fixo, herdado da sensibilidade).
set.seed(42)

dados_base_imputado <-
  dados_base |>
  select(-id_hex) |>
  recipe(is_slum ~ .) |>
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  prep() |>
  bake(new_data = NULL)

df_balanceado_global <-
  themis::smotenc(
    dados_base_imputado,
    var = "is_slum",
    over_ratio = 0.5
  )

hash_linha <- function(df) {
  df |>
    mutate(across(everything(), as.character)) |>
    tidyr::unite("hash_key", everything(), sep = "|") |>
    pull(hash_key)
}

hashes_reais <- hash_linha(dados_base_imputado)

df_balanceado_global <-
  df_balanceado_global |>
  mutate(is_synthetic = !hash_linha(df_balanceado_global) %in% hashes_reais)

n_sintetico_total <- sum(df_balanceado_global$is_synthetic)
message(sprintf(
  ">>> %d de %d linhas marcadas como sintéticas (%.1f%%).",
  n_sintetico_total,
  nrow(df_balanceado_global),
  100 * n_sintetico_total / nrow(df_balanceado_global)
))

set.seed(123) # mesma seed da CV honesta (Seção 5 do 01_model_aricle.R)
folds_leak <-
  vfold_cv(df_balanceado_global, v = 10, strata = is_slum)

fracao_sintetica_por_fold <-
  purrr::map_dbl(folds_leak$splits, function(split) {
    avaliacao <- rsample::assessment(split)
    slum_avaliacao <- avaliacao |> filter(is_slum == "Slum")
    if (nrow(slum_avaliacao) == 0) {
      return(NA_real_)
    }
    mean(slum_avaliacao$is_synthetic)
  })

fracao_sintetica_media <- mean(fracao_sintetica_por_fold, na.rm = TRUE)
message(sprintf(
  ">>> Fração média de 'Slum' sintético nos folds de validação (10-fold, ",
  "Tabela B.5): %.1f%%.",
  100 * fracao_sintetica_media
))

receita_leak <-
  recipe(is_slum ~ ., data = df_balanceado_global) |>
  step_rm(is_synthetic) |> # marcador diagnóstico — não é preditor
  step_corr(all_numeric_predictors(), threshold = 0.90, method = "spearman")

wf_set_leak <-
  workflow_set(
    preproc = list(receita = receita_leak),
    models = especificacoes_modelos
  )

resultados_leak <-
  workflow_map(
    wf_set_leak,
    fn = "fit_resamples",
    resamples = folds_leak,
    metrics = metric_set(accuracy, precision, recall, roc_auc, f_meas),
    control = control_resamples(save_pred = FALSE)
  )

tabela_b5_leak <-
  collect_metrics(resultados_leak) |>
  select(wflow_id, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  arrange(desc(recall)) |>
  select(wflow_id, accuracy, precision, recall, roc_auc, f_meas) |>
  mutate(
    wflow_id = str_remove(wflow_id, "^receita_")
  )

message("\n=== Tabela B.5 'com leakage' (SMOTENC antes do split) ===")
print(tabela_b5_leak, n = Inf)

# ==============================================================================
# 5. COMPARAÇÃO TRIPLA: ARTIGO x HONESTO (01_model_aricle.R) x LEAKAGE
# ==============================================================================
tabela_artigo_b5 <- tibble::tribble(
  ~wflow_id, ~accuracy, ~precision, ~recall, ~roc_auc, ~f_meas,
  "decision_tree", 0.93, 0.89, 0.91, 0.92, 0.90,
  "random_forest", 0.96, 0.93, 0.95, 0.99, 0.94,
  "gradient_boosting", 0.93, 0.89, 0.91, 0.98, 0.89,
  "adaboost", 0.91, 0.86, 0.88, 0.97, 0.87,
  "xgboost", 0.95, 0.92, 0.94, 0.99, 0.93,
  "knn", 0.91, 0.90, 0.96, 0.96, 0.88
) |>
  mutate(origem = "Artigo (Tabela B.5)")

tabela_b5_honesta_fmt <-
  tabela_b5_honesta |>
  mutate(wflow_id = str_remove(wflow_id, "^receita_")) |>
  rename(roc_auc = roc_auc, f_meas = f_meas) |>
  mutate(origem = "Honesto (01_model_aricle.R, SMOTENC só no treino)")

tabela_b5_leak_fmt <-
  tabela_b5_leak |>
  mutate(origem = "Com leakage (SMOTENC antes do split)")

comparacao_b5 <-
  bind_rows(tabela_artigo_b5, tabela_b5_honesta_fmt, tabela_b5_leak_fmt) |>
  select(origem, wflow_id, accuracy, precision, recall, roc_auc, f_meas) |>
  arrange(wflow_id, origem)

message("\n=== Comparação: Artigo x Honesto x Leakage, por algoritmo ===")
print(comparacao_b5, n = Inf)

# Destaque específico para o KNN — o sinal de alerta que motivou este teste
message("\n=== Foco: KNN (o mais suspeito na Tabela B.5 original) ===")
print(comparacao_b5 |> filter(wflow_id == "knn"), n = Inf)

# ==============================================================================
# 6. EXPORTAÇÃO
# ==============================================================================
dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)

artefato_leakage_b5 <- list(
  tabela_b5_leak = tabela_b5_leak,
  comparacao_b5 = comparacao_b5,
  fracao_sintetica_media = fracao_sintetica_media,
  fracao_sintetica_por_fold = fracao_sintetica_por_fold,
  pct_linhas_sinteticas_total = 100 *
    n_sintetico_total /
    nrow(df_balanceado_global)
)

saveRDS(
  artefato_leakage_b5,
  file.path(pasta_saida, paste0("leakage_demo_tabela_b5", sufixo_saida, ".rds"))
)

readr::write_csv(
  comparacao_b5,
  file.path(
    pasta_saida,
    "tables",
    paste0("comparacao_tabela_b5_leakage", sufixo_saida, ".csv")
  )
)

# ==============================================================================
# 7. LEITURA DOS RESULTADOS — O QUE PROCURAR
# ==============================================================================
# 1. 'fracao_sintetica_media': se estiver alta (na faixa que já vimos para
#    a Tabela A.4, 68-91% dependendo do limiar), isso sozinho já é
#    evidência de que o mesmo tipo de vazamento é estruturalmente possível
#    aqui.
# 2. 'comparacao_b5': para cada algoritmo, compare Precision/Recall/F1 nas
#    três origens. Se os números "com leakage" ficarem visivelmente mais
#    próximos do artigo do que os "honestos" — em TODOS os 6 algoritmos,
#    não só num — isso sustenta a hipótese de que o mesmo erro
#    metodológico (SMOTENC antes do split) também contaminou a escolha do
#    modelo final, não só o limiar de cobertura.
# 3. Preste atenção especial no KNN: se o recall do KNN "com leakage" saltar
#    de forma desproporcional em relação aos outros 5 algoritmos (mais do
#    que a diferença honesto-vs-leakage nos demais), isso é uma assinatura
#    ainda mais específica de vazamento por proximidade/quase-duplicação —
#    KNN reage a esse tipo de vazamento de um jeito que árvores não reagem
#    tão facilmente.
# FIM ==========================================================================
