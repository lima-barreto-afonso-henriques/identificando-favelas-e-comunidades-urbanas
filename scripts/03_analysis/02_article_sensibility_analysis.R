# ==============================================================================
# ANÁLISE DE SENSIBILIDADE — LIMIAR DE COBERTURA DE FAVELA
# Versão: REPLICAÇÃO FIEL DO ARTIGO (Apêndice A / Seção 3.3-3.4)
# ==============================================================================

# ==============================================================================
# 0. PACOTES E CAMINHOS
# ==============================================================================
gc()

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada/saída
pasta_entrada <- here("data", "processed")
pasta_saida <- here("data", "outputs")

# ==============================================================================
# 1. CARREGAR DADOS
# ==============================================================================
grid_hexagonal <-
  readRDS(
    file.path(
      pasta_entrada,
      "grid_hexagonal_artigo.rds"
    )
  )
glimpse(grid_hexagonal)

# ==============================================================================
# 2. PRÉ-PROCESSAMENTO (alinhado ao artigo)
# ==============================================================================
grid_hexagonal <-
  grid_hexagonal |>
  # UTM
  st_transform(31983) |>
  # Seleção explícita de preditores — evita que colunas de metadado
  # administrativo (ou um eventual rótulo alternativo) entrem como
  # preditoras sem querer.
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
  # filtro dasimétrico básico
  filter(number_private_residences > 0) |>
  mutate(
    across(
      where(is.logical),
      ~ factor(., levels = c(FALSE, TRUE))
    )
  )

grid_df <-
  grid_hexagonal |>
  st_drop_geometry()

skimr::skim(grid_df)

# ==============================================================================
# 3. CONFIGURAÇÃO DA MODELAGEM (Decision Tree + SMOTENC) E PARALELIZAÇÃO
# ==============================================================================
# Especificação do Modelo
# Decision Tree
dt_spec <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

# Limiares de 10% a 70% em incrementos de 10%
limiares_teste <- seq(0.1, 0.7, by = 0.1)

options(parallelly.makeNodePSOCK.setup_strategy = "sequential")
n_workers <- max(1, parallelly::availableCores() - 1)

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
# 4. LOOP DE SENSIBILIDADE E 100-FOLD CV
# ==============================================================================
# Iniciando testes de sensibilidade (100-fold CV)

resultados_folds <-
  map_dfr(limiares_teste, function(limiar) {
    # 4.1. Criar a variável alvo para o limiar atual
    df_model <-
      grid_df |>
      mutate(
        is_slum = factor(
          if_else(slum_coverage_pct >= limiar, "Slum", "Non_Slum"),
          levels = c("Slum", "Non_Slum")
        )
      )

    # 4.2. Criar os 100 folds (fixo com seed para comparabilidade justa par a par)
    set.seed(42)
    folds_100 <-
      vfold_cv(df_model, v = 100)

    # 4.3. Receita com filtro de correlação (Seção 3.4 do artigo) + SMOTENC
    # (over_ratio não especificado no texto, padrão 0.5 assumido — mesma
    # razão de 50% descrita na Seção 3.5: "stratified sampling strategy was
    # applied with a 50% ratio")
    receita <-
      recipe(is_slum ~ ., data = df_model) |>
      update_role(id_hex, new_role = "id") |>
      step_rm(slum_coverage_pct) |>
      step_impute_median(all_numeric_predictors()) |>
      step_zv(all_predictors()) |>
      # filtro de correlação > 0.9 (Seção 3.4 do artigo)
      step_corr(
        all_numeric_predictors(),
        threshold = 0.90,
        method = "spearman"
      ) |>
      step_smotenc(is_slum, over_ratio = 0.5)

    wf <-
      workflow() |>
      add_recipe(receita) |>
      add_model(dt_spec)

    # 4.4. Treinamento e avaliação — conjunto completo de métricas para bater
    # com a Tabela A.4 do artigo (Accuracy, Precision, Recall, ROC AUC, F1),
    # + Specificity como diagnóstico extra (permite derivar FPR = 1 - spec,
    # usado no texto do artigo: "1.18% false positive rate" no limiar de 70%)
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

    # Coletar os resultados não sumarizados (todos os 100 folds)
    collect_metrics(resamples_fit, summarize = FALSE) |>
      mutate(limiar = limiar)
  })
resultados_folds
#
future::plan(sequential)
#
# ==============================================================================
# 5. RECONSTRUÇÃO DA TABELA A.4 (médias das métricas por limiar)
# ==============================================================================
tabela_A4 <-
  resultados_folds |>
  group_by(limiar, .metric) |>
  summarise(media = mean(.estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = .metric, values_from = media) |>
  mutate(
    FPR = 1 - spec, # False Positive Rate = 1 - Specificity (diagnóstico extra)
    # round() evita erro de ponto flutuante em 'limiar * 100'
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

message("\n=== Tabela A.4 Reconstruída (formato do artigo) ===")
print(tabela_A4 |> select(-limiar))

# ==============================================================================
# 6. TESTE DE MANN-WHITNEY U E FIGURA A.12
# ==============================================================================
# Isolar apenas as distribuições de Recall (100 valores por limiar)
recall_dists <- resultados_folds |> filter(.metric == "recall")

# Expandir matriz de combinações para triângulo inferior
combinacoes_mw <-
  expand_grid(t1 = limiares_teste, t2 = limiares_teste) |>
  filter(t1 > t2) |> # garante que seja um triângulo inferior
  rowwise() |>
  mutate(
    # Aplicar Wilcoxon Rank Sum Test (Mann-Whitney U) entre as distribuições
    p_value = wilcox.test(
      recall_dists$.estimate[recall_dists$limiar == t1],
      recall_dists$.estimate[recall_dists$limiar == t2],
      exact = FALSE
    )$p.value,
    significant = p_value < 0.05
  ) |>
  ungroup() |>
  mutate(
    t1_label = paste0(round(t1 * 100), "%"),
    t2_label = paste0(round(t2 * 100), "%")
  )

# Converter labels para fatores ordenados para plotagem correta
niveis <- paste0(round(limiares_teste * 100), "%")

combinacoes_mw$t1_label <- factor(combinacoes_mw$t1_label, levels = niveis)
combinacoes_mw$t2_label <- factor(combinacoes_mw$t2_label, levels = niveis)

# Plotar a Matriz (Reprodução da Fig. A.12)
plot_fig_A12 <- ggplot(
  combinacoes_mw,
  aes(x = t2_label, y = t1_label, fill = significant)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("TRUE" = "#a6d96a", "FALSE" = "#542788"),
    labels = c("Significant", "Not Significant"),
    name = "Statistical Significance\n(p < 0.05)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Mann-Whitney Test P-values Matrix (Lower Triangle)",
    subtitle = "Decision Tree | 100-fold CV | Recall Distributions"
  )

print(plot_fig_A12)

# ==============================================================================
# 7. SELEÇÃO DO LIMIAR: FIXO (ARTIGO, 70%) x VERIFICAÇÃO EMPÍRICA
# ==============================================================================
# O artigo escolhe 70% citando "the best trade-off, achieving a recall of 92%
# and a false positive rate of 1.18%" — ou seja, o critério declarado não é
# "recall máximo isolado", é um TRADE-OFF entre recall e FPR. O equivalente
# estatístico padrão para esse trade-off é o J de Youden
# (J = Sensibilidade + Especificidade - 1 = Recall - FPR), que soma 1 se o
# classificador for perfeito e 0 se for equivalente a chute aleatório.
tabela_A4 <-
  tabela_A4 |>
  mutate(youden_j = Recall - FPR)

limiar_artigo <- "limiar_70"

metricas_limiar_artigo <-
  tabela_A4 |>
  filter(Threshold == limiar_artigo) |>
  mutate(criterio = "Fixo (artigo, 70% de cobertura)")

melhor_por_recall <-
  tabela_A4 |>
  slice_max(Recall, n = 1, with_ties = FALSE) |>
  mutate(criterio = "Melhor empírico — maior Recall isolado")

melhor_por_trade_off <-
  tabela_A4 |>
  slice_max(youden_j, n = 1, with_ties = FALSE) |>
  mutate(criterio = "Melhor empírico — maior J de Youden (Recall - FPR)")

comparacao_limiares <-
  bind_rows(metricas_limiar_artigo, melhor_por_recall, melhor_por_trade_off) |>
  select(
    criterio,
    Threshold,
    Recall,
    FPR,
    youden_j,
    Accuracy,
    Precision,
    ROC_AUC,
    F1
  )

message("\n=== Tabela A.4 reconstruída (com J de Youden) ===")
print(tabela_A4 |> select(-limiar))

message(
  "\n=== Comparação: limiar fixo do artigo x melhores limiares empíricos ==="
)
print(comparacao_limiares)

if (nrow(metricas_limiar_artigo) == 0) {
  warning(
    "O limiar '",
    limiar_artigo,
    "' não foi encontrado em 'tabela_A4' — ",
    "verifique se 'limiares_teste' realmente inclui 0.7."
  )
} else if (
  metricas_limiar_artigo$Threshold[1] == melhor_por_recall$Threshold[1] &&
    metricas_limiar_artigo$Threshold[1] == melhor_por_trade_off$Threshold[1]
) {
  message(
    "\n>>> O limiar de 70% do artigo COINCIDE com o melhor resultado empírico ",
    "nesta execução (tanto por recall quanto por trade-off). Consistente com o artigo."
  )
} else {
  message(
    "\n>>> ATENÇÃO: o limiar de 70% do artigo NÃO coincide com o melhor resultado ",
    "empírico nesta execução. Isso pode refletir diferenças no algoritmo, na ",
    "amostra (N = ",
    nrow(grid_df),
    " hexágonos), ou variação ",
    "amostral genuína — inspecione 'comparacao_limiares' antes de decidir qual ",
    "limiar levar ao script do modelo final."
  )
}

print(metricas_limiar_artigo)

# ==============================================================================
# 8. EXPORTAÇÃO DOS ARTEFATOS
# ==============================================================================
artefato_artigo <- list(
  tabela_A4 = tabela_A4,
  testes_mann_whitney = combinacoes_mw,
  grafico_A12 = plot_fig_A12,
  raw_100_folds = resultados_folds,
  comparacao_limiares = comparacao_limiares,
  limiar_escolhido = melhor_por_trade_off$Threshold[1]
)

# [FIX-3] 'pasta_saida' (não 'caminho_saida', que nunca existiu neste script)
saveRDS(
  artefato_artigo,
  file.path(pasta_saida, "sensibilidade_artigo.rds")
)

message(
  "\nScript finalizado e artefato salvo em '",
  file.path(pasta_saida, "sensibilidade_artigo.rds"),
  "'."
)

# FIM ==========================================================================
