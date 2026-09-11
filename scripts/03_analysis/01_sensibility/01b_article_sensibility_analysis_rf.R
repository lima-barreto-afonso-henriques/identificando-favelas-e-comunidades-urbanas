# ==============================================================================
# ANÁLISE DE SENSIBILIDADE — LIMIAR DE COBERTURA DE FAVELA
# Versão: RANDOM FOREST (modelo final do artigo) — SMOTENC SEM leakage
# ==============================================================================
# DIDÁTICA:
# Esta é uma variação do 02_article_sensibility_analysis.R trocando apenas o
# algoritmo: Decision Tree -> Random Forest, que é o modelo que o artigo
# efetivamente usa na Seção 4.2 ("The Random Forest algorithm was selected
# as the final model"). O Apêndice A (Tabela A.4) descreve a sensibilidade
# usando "a decision tree classifier" — ou seja, o artigo testou o LIMIAR
# com um modelo mais simples e barato (decision tree) e só depois treinou
# o modelo final (Random Forest) no limiar escolhido.
#
# Rodar a sensibilidade também com Random Forest serve para checar se a
# escolha do limiar de 70% se sustenta com o modelo que de fato vai para
# produção — e para comparar se o Random Forest, por si só (mais robusto
# a ruído que uma única árvore), já reduz a diferença que você observou
# em relação à Tabela A.4, SEM precisar recorrer à hipótese de leakage.
#
# O pipeline de pré-processamento, folds e métricas é IDÊNTICO ao script 02
# (mesma seed, mesmos 100 folds, mesma recipe, SMOTENC aplicado apenas
# dentro de cada fold de TREINO — comportamento correto e sem leakage).
# ==============================================================================

# ==============================================================================
# 0. PACOTES E CAMINHOS
# ==============================================================================
gc()

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

pasta_entrada <- here("data", "processed")
pasta_saida <- here("outputs", "artefato")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ==============================================================================
# 1. CARREGAR DADOS
# ==============================================================================
# a resolução está fixada para rodar esse
# script individualmente para cada resolução (9, 10 ou 11).
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 11

arquivos_entrada <- c(
  "9" = "grid_hexagonal_res9.rds",
  "10" = "grid_hexagonal_artigo.rds",
  "11" = "grid_hexagonal_res11.rds"
)

# GRID HEXAGONAL -------------------------------------------------------------------
sufixo_saida <- paste0("_res", resolucao_hex)

# opçoes: area_pura ou ponderado_edificações
# Defina a opção desejada: "area_pura" ou "ponderado_edificacoes"
tipo_grid <- "area_pura" # ou "ponderado_edificacoes"

caminho_grid_hexagonal <- file.path(
  pasta_entrada,
  paste0("grid_hexagonal_", tipo_grid, sufixo_saida, ".rds")
)

grid_hexagonal <-
  readRDS(caminho_grid_hexagonal)
glimpse(grid_hexagonal)


# ==============================================================================
# 2. PRÉ-PROCESSAMENTO (idêntico ao script 02)
# ==============================================================================
grid_hexagonal <-
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
  filter(number_private_residences > 0) |>
  mutate(
    across(
      where(is.logical),
      ~ factor(., levels = c(FALSE, TRUE))
    )
  )

# Limpeza de atributos residuais de sf
# (idêntico ao script 01a — barata e evita payload extra na
# serialização para os workers do future)
limpar_atributos_sf <- function(df) {
  df <- as.data.frame(df)
  attrs_para_remover <- c("sf_column", "agr", "relation_to_geometry")
  for (a in attrs_para_remover) {
    attr(df, a) <- NULL
  }
  class(df) <- "data.frame"
  tibble::as_tibble(df)
}

grid_df <-
  grid_hexagonal |>
  st_drop_geometry() |>
  limpar_atributos_sf()

skimr::skim(grid_df)

# ==============================================================================
# 3. PARALELIZAÇÃO
# ==============================================================================
# DIDÁTICA: Random Forest com 100 folds x 7 limiares é MUITO mais pesado que
# Decision Tree (cada fold treina N árvores, não 1). Reduza 'trees' abaixo
# se a execução ficar impraticável na sua máquina.

options(parallelly.makeNodePSOCK.setup_strategy = "sequential")
n_workers <- max(1, min(6, parallelly::availableCores() - 1))

tryCatch(
  future::plan(multisession, workers = n_workers),
  error = function(e) {
    message(
      "Não foi possível iniciar processamento em paralelo (",
      conditionMessage(e),
      "). Rodando sequencialmente."
    )
    future::plan(sequential)
  }
)

# ==============================================================================
# 4. ESPECIFICAÇÃO DO MODELO — RANDOM FOREST
# ==============================================================================
# DIDÁTICA:
# O artigo não detalha hiperparâmetros do Random Forest final além de citar
# um "Grid Search" (Seção 3.5) sem especificar o espaço de busca. Aqui usamos
# valores default do ranger (mtry = sqrt(p), min_n = 1 para classificação)
# como ponto de partida honesto e documentado — não uma tentativa de
# reproduzir hiperparâmetros que o artigo não publicou.
rf_spec <-
  rand_forest(trees = 500) |>
  set_engine("ranger", importance = "impurity", seed = 42) |>
  set_mode("classification")

# ==============================================================================
# 5. FUNÇÃO DE AVALIAÇÃO MODULAR (idêntica em estrutura ao script 02)
# ==============================================================================
avaliar_limiar <- function(limiar, data, model_spec) {
  df_model <-
    data |>
    mutate(
      is_slum = factor(
        if_else(slum_coverage_pct >= limiar, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      )
    ) |>
    limpar_atributos_sf()

  set.seed(42)
  folds_100 <-
    vfold_cv(df_model, v = 100)

  # Receita — SMOTENC dentro do step, ou seja, o tidymodels/recipes
  # aplica o balanceamento SOMENTE nos dados de TREINO de cada fold,
  # nunca tocando o fold de validação. Isso é o comportamento correto
  # e é o que diferencia este script do 03_data_leakage_demo.R.
  receita <-
    recipe(is_slum ~ ., data = df_model) |>
    update_role(id_hex, new_role = "id") |>
    step_rm(slum_coverage_pct) |>
    step_impute_median(all_numeric_predictors()) |>
    step_zv(all_predictors()) |>
    step_corr(
      all_numeric_predictors(),
      threshold = 0.90,
      method = "spearman"
    ) |>
    step_smotenc(is_slum, over_ratio = 0.5)

  wf <-
    workflow() |>
    add_recipe(receita) |>
    add_model(model_spec)

  resamples_fit <-
    fit_resamples(
      wf,
      resamples = folds_100,
      metrics = metric_set(
        yardstick::accuracy,
        yardstick::precision,
        yardstick::recall,
        yardstick::spec,
        yardstick::roc_auc,
        yardstick::f_meas
      ),
      control = control_resamples(save_pred = FALSE)
    )

  collect_metrics(
    resamples_fit,
    summarize = FALSE
  ) |>
    mutate(limiar = limiar)
}

# ==============================================================================
# 6. EXECUÇÃO DA SENSIBILIDADE
# ==============================================================================
limiares_teste <- seq(0.1, 0.7, by = 0.1)

distribuicao_limiares <- purrr::map_dfr(limiares_teste, function(l) {
  grid_df |>
    summarise(
      limiar = l,
      threshold = paste0("limiar_", l * 100),
      qtd_slums = sum(slum_coverage_pct >= l),
      total_hexagons = n(),
      pct_slums = (qtd_slums / total_hexagons) * 100
    )
})

print(distribuicao_limiares)

resultados_folds_rf <- purrr::map_dfr(
  limiares_teste,
  ~ avaliar_limiar(
    limiar = .x,
    data = grid_df,
    model_spec = rf_spec
  )
)
#
future::plan(sequential)
#
#
# ==============================================================================
# 7. RECONSTRUÇÃO DA TABELA A.4 (versão Random Forest)
# ==============================================================================
tabela_A4_rf <-
  resultados_folds_rf |>
  group_by(limiar, .metric) |>
  summarise(media = mean(.estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = .metric, values_from = media) |>
  mutate(
    FPR = 1 - spec,
    Threshold = paste0("limiar_", round(limiar * 100))
  ) |>
  arrange(limiar) |>
  rename(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    ROC_AUC = roc_auc,
    F1 = f_meas,
    Specificity = spec
  ) |>
  select(
    Threshold,
    limiar,
    Accuracy,
    Precision,
    Recall,
    ROC_AUC,
    F1,
    Specificity,
    FPR
  )

print(
  tabela_A4_rf |>
    select(-limiar)
)

# ==============================================================================
# 7b. Atualiza a pasta de saída para incluir a subpasta do tipo de grid
# ==============================================================================
# Movido para antes da Seção 8: o 01a salva o artefato de sensibilidade
# dentro de pasta_saida/tipo_grid/, então a comparação abaixo precisa
# resolver o mesmo caminho para encontrar o arquivo. Também precisamos
# das subpastas tables/figures já criadas antes do write_csv da Seção 8.
pasta_saida <- file.path(pasta_saida, tipo_grid)
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)
dir.create(file.path(pasta_saida, "figures"), showWarnings = FALSE)

# ==============================================================================
# 8. COMPARAÇÃO DIRETA: DECISION TREE (script 02) x RANDOM FOREST (este script)
# ==============================================================================
# DIDÁTICA: se você já tem 'tabela_A4' salva do script 02 no ambiente (ou
# recarregada do .rds em outputs/artefato/<tipo_grid>/sensibilidade_artigo_resXX.rds),
# este bloco junta as duas para comparação lado a lado no limiar de 70%.
caminho_artefato_02 <- file.path(
  pasta_saida,
  paste0("sensibilidade_artigo", sufixo_saida, ".rds")
)

if (file.exists(caminho_artefato_02)) {
  artefato_02 <- readRDS(caminho_artefato_02)

  comparacao_dt_rf <-
    bind_rows(
      artefato_02$tabela_A4 |>
        select(-limiar) |>
        mutate(modelo = "Decision Tree (script 02)"),
      tabela_A4_rf |>
        select(-limiar) |>
        mutate(modelo = "Random Forest (este script)")
    ) |>
    select(modelo, Threshold, Accuracy, Precision, Recall, ROC_AUC, F1, FPR)

  print(comparacao_dt_rf)

  readr::write_csv(
    comparacao_dt_rf,
    file.path(pasta_saida, "tables", "comparacao_dt_vs_rf.csv")
  )
} else {
  message(
    "Artefato do script 02 não encontrado em '",
    caminho_artefato_02,
    "'. Rode o 02_article_sensibility_analysis.R primeiro para habilitar ",
    "a comparação Decision Tree x Random Forest."
  )
}

# ==============================================================================
# 9. EXPORTAÇÃO DOS ARTEFATOS
# ==============================================================================
# pasta_saida (com subpasta tipo_grid) e as subpastas tables/figures já
# foram criadas na Seção 7b, antes da comparação DT x RF.

# 9.1. O "Cofre" Reprodutível (Para RMarkdown / Quarto)
artefato_rf <- list(
  tabela_A4_rf = tabela_A4_rf,
  raw_100_folds_rf = resultados_folds_rf
)

# 9.2. Exportação para Submissão/Leitura Humana (Arquivos Avulsos)
saveRDS(
  artefato_rf,
  file.path(
    pasta_saida,
    paste0("sensibilidade_artigo_rf", sufixo_saida, ".rds")
  )
)

readr::write_csv(
  tabela_A4_rf,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_A4_random_forest", sufixo_saida, ".csv")
  )
)

# FIM ==========================================================================
