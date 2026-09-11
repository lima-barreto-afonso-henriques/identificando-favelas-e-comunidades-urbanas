# ==============================================================================
# DEMONSTRAÇÃO DE VAZAMENTO DE DADOS NO HOLDOUT DE TESTE DO MODELO FINAL
# (Random Forest, split 80/20, Seção 4.2 / Fig. 4a do artigo)
# ==============================================================================
# OBJETIVO DESTE SCRIPT:
# Diferente de 03a/03b (Tabela A.4) e 03c (Tabela B.5), aqui a hipótese
# testada é mais específica e mais grave: o SMOTENC teria sido aplicado
# ANTES do split 80/20 em si (não só antes das CVs de sensibilidade/
# comparação de algoritmo) — contaminando diretamente o conjunto de teste
# que o artigo reporta como resultado PRINCIPAL (recall = 86.45%, Fig. 4a).
#
# POR QUE TESTAR ISSO SEPARADAMENTE (e não assumir que é igual a 03a/03c):
# O texto do artigo descreve o split 80/20 numa frase estruturalmente
# DIFERENTE da frase ambígua do SMOTENC/CV:
#   "The dataset was randomly split into 80% of the data for training and
#    20% for testing, preventing overfitting" (Seção 3.5)
# Isso vem ANTES, num parágrafo separado da menção ao SMOTENC/resample em
# 10 folds — ao contrário das Tabelas A.4/B.5, não há a mesma ambiguidade
# textual direta aqui. Além disso, RECONSTRUINDO os números reais da
# matriz de confusão do teste (Fig. 4a: TN=9359, FP=1125, FN=66, TP=421):
#     Recall    = 421/(421+66)   = 86.45% (bate com o texto)
#     Precision = 421/(421+1125) = 27.23%
#     Accuracy  = (9359+421)/10971 = 89.15%
#     F1        ~ 41.4%
# Essa Precision de 27% é muito PARECIDA com a do seu Decision Tree
# honesto (27.2% no limiar 70%, script 02) — nada parecido com os 92% "bons
# demais" da Tabela A.4. Isso já é evidência de que o holdout de teste do
# modelo final NÃO carrega a mesma assinatura de vazamento das tabelas de
# sensibilidade/comparação. Este script testa essa leitura diretamente,
# em vez de simplesmente assumir que "se vazou lá, vazou aqui também".
#
# COMO USAR:
# Rode DEPOIS do 01_model_aricle.R (mesma 'resolucao_hex') — reaproveita o
# limiar herdado e os hiperparâmetros já otimizados pelo Grid Search
# (Seção 7 daquele script), para isolar APENAS o efeito do vazamento,
# sem misturar com diferença de hiperparâmetros.
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

resolucao_hex <- 10 # <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
sufixo_saida <- paste0("_res", resolucao_hex)

# ==============================================================================
# 1. IMPORTAÇÃO — LIMIAR E HIPERPARÂMETROS HERDADOS DO 01_model_aricle.R
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
    "definida aqui) — este script herda o limiar e os hiperparâmetros ",
    "otimizados de lá, para isolar apenas o efeito do vazamento."
  )
}

artefato_modelo <- readRDS(caminho_artefato_modelo)
limiar_fracao <- artefato_modelo$limiar_decisao_artigo
melhores_hiperparametros <- artefato_modelo$melhores_hiperparametros

message(sprintf(
  ">>> Limiar herdado: %.0f%% | mtry=%s, trees=%s, min_n=%s (herdados do Grid Search honesto)",
  limiar_fracao * 100,
  melhores_hiperparametros$mtry,
  melhores_hiperparametros$trees,
  melhores_hiperparametros$min_n
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

# ==============================================================================
# 2. PASSO PROBLEMÁTICO: SMOTENC ANTES DO SPLIT 80/20 (não só antes da CV)
# ==============================================================================
# DIDÁTICA — DIFERENÇA-CHAVE em relação a 03a/03c: lá, o SMOTENC ia antes
# de vfold_cv() (contaminando as validações internas). Aqui, o SMOTENC vai
# antes de initial_split() — contaminando diretamente o CONJUNTO DE TESTE
# reportado como resultado principal do artigo (Fig. 4a). É um vazamento
# estruturalmente mais grave, se tiver ocorrido, porque afeta o número que
# o artigo mais enfatiza (recall = 86.45%), não uma tabela auxiliar.
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

message(sprintf(
  ">>> %d de %d linhas marcadas como sintéticas (%.1f%%) — inclusive no ",
  "conjunto que vai virar 'teste' a seguir.",
  sum(df_balanceado_global$is_synthetic),
  nrow(df_balanceado_global),
  100 *
    sum(df_balanceado_global$is_synthetic) /
    nrow(df_balanceado_global)
))

# Split 80/20 SOBRE A BASE JÁ CONTAMINADA — o próprio "teste" pode conter
# sintéticos cujos geradores reais caíram no "treino".
set.seed(42) # mesma seed do split honesto (Seção 3 do 01_model_aricle.R)
split_leak <- initial_split(
  df_balanceado_global,
  prop = 0.80,
  strata = is_slum
)
treino_leak <- training(split_leak)
teste_leak <- testing(split_leak)

fracao_sintetica_teste <-
  teste_leak |>
  filter(is_slum == "Slum") |>
  summarise(fracao = mean(is_synthetic)) |>
  pull(fracao)

message(sprintf(
  ">>> Fração de 'Slum' sintético dentro do conjunto de TESTE (não CV, o ",
  "holdout que o artigo reporta como resultado principal): %.1f%%.",
  100 * fracao_sintetica_teste
))

# ==============================================================================
# 3. TREINO E AVALIAÇÃO — MESMOS HIPERPARÂMETROS DO MODELO HONESTO
# ==============================================================================
receita_leak <-
  recipe(is_slum ~ ., data = treino_leak) |>
  step_rm(is_synthetic) |>
  step_corr(all_numeric_predictors(), threshold = 0.90, method = "spearman")

spec_rf_leak <-
  rand_forest(
    mtry = melhores_hiperparametros$mtry,
    trees = melhores_hiperparametros$trees,
    min_n = melhores_hiperparametros$min_n
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

wf_leak <-
  workflow() |>
  add_recipe(receita_leak) |>
  add_model(spec_rf_leak)

fit_leak <- fit(wf_leak, data = treino_leak)

preds_teste_leak <-
  predict(fit_leak, new_data = teste_leak, type = "class") |>
  bind_cols(teste_leak |> select(is_slum))

matriz_confusao_leak <-
  preds_teste_leak |>
  conf_mat(truth = is_slum, estimate = .pred_class)

print(matriz_confusao_leak)

metricas_teste_leak <-
  bind_rows(
    accuracy(preds_teste_leak, truth = is_slum, estimate = .pred_class),
    precision(preds_teste_leak, truth = is_slum, estimate = .pred_class),
    recall(preds_teste_leak, truth = is_slum, estimate = .pred_class),
    f_meas(preds_teste_leak, truth = is_slum, estimate = .pred_class)
  ) |>
  select(.metric, .estimate) |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  mutate(origem = "Com leakage (SMOTENC antes do split 80/20)")

# ==============================================================================
# 4. COMPARAÇÃO: ARTIGO (RECONSTRUÍDO DA FIG. 4a) x HONESTO x LEAKAGE
# ==============================================================================
# Reconstrução exata a partir da matriz de confusão publicada (Fig. 4a):
# TN=9359, FP=1125, FN=66, TP=421 (test set, 20% dos dados, 10971 linhas)
tn <- 9359
fp <- 1125
fn <- 66
tp <- 421

tabela_artigo_teste <- tibble(
  origem = "Artigo (reconstruído da Fig. 4a)",
  accuracy = (tp + tn) / (tp + tn + fp + fn),
  precision = tp / (tp + fp),
  recall = tp / (tp + fn),
  f_meas = 2 * (tp / (tp + fp)) *
    (tp / (tp + fn)) /
    ((tp / (tp + fp)) + (tp / (tp + fn)))
)

# Métricas honestas do 01_model_aricle.R — ajuste o nome do campo abaixo
# se a estrutura salva por aquele script usar outro nome para a tabela do
# holdout de teste (ex.: 'tabela_comparativa_teste').
tabela_honesta_teste <-
  artefato_modelo$tabela_comparativa_teste |>
  mutate(origem = "Honesto (01_model_aricle.R)")

comparacao_teste_final <-
  bind_rows(
    tabela_artigo_teste,
    tabela_honesta_teste,
    metricas_teste_leak
  ) |>
  select(origem, accuracy, precision, recall, f_meas)

message("\n=== Comparação: Artigo x Honesto x Leakage — holdout de teste (RF final) ===")
print(comparacao_teste_final, n = Inf)

# ==============================================================================
# 5. EXPORTAÇÃO
# ==============================================================================
artefato_leakage_teste <- list(
  matriz_confusao_leak = matriz_confusao_leak,
  metricas_teste_leak = metricas_teste_leak,
  tabela_artigo_teste_reconstruida = tabela_artigo_teste,
  comparacao_teste_final = comparacao_teste_final,
  fracao_sintetica_teste = fracao_sintetica_teste
)

saveRDS(
  artefato_leakage_teste,
  file.path(
    pasta_saida,
    paste0("leakage_demo_holdout_final", sufixo_saida, ".rds")
  )
)

dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)
readr::write_csv(
  comparacao_teste_final,
  file.path(
    pasta_saida,
    "tables",
    paste0("comparacao_holdout_final_leakage", sufixo_saida, ".csv")
  )
)

# ==============================================================================
# 6. LEITURA DOS RESULTADOS — O QUE PROCURAR
# ==============================================================================
# 1. Se 'metricas_teste_leak' (Precision especialmente) ficar bem mais
#    próxima dos 92% da Tabela A.4 do que dos 27% reconstruídos da Fig. 4a
#    — isso sugeriria que o MESMO tipo de vazamento também poderia
#    explicar números "bons demais" SE o artigo tivesse reportado assim.
#    Mas como o artigo já reporta ~27% de Precision no teste (não ~92%),
#    a leitura mais provável é que o holdout de teste real do artigo NÃO
#    sofreu esse vazamento — mesmo que as Tabelas A.4/B.5 tenham sofrido.
# 2. 'fracao_sintetica_teste': mesmo que dê um número alto (esperado, dado
#    o padrão já visto), isso mostra que o MECANISMO é possível — não que
#    ele necessariamente ocorreu no holdout real, já que os números
#    publicados do teste já parecem honestos.
# 3. Este script serve principalmente para checar se as métricas honestas
#    e as com leakage DIVERGEM (evidência de que o mecanismo importa) e
#    para confirmar, por eliminação, que o número mais citado do artigo
#    (recall 86.45%) provavelmente não é produto do mesmo vazamento das
#    tabelas de sensibilidade/comparação de algoritmo.
# FIM ==========================================================================
