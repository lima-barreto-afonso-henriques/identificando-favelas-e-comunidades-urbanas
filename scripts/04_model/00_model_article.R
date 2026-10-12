# ==============================================================================
# MODELO FINAL — LINHA REPRODUÇÃO (fiel ao artigo) — v3
# Segue os passos descritos nas Seções 3.5-3.8 e Apêndice B do artigo:
#   - Comparação de 6 algoritmos via CV (Tabela B.5)
#   - Random Forest selecionado (maior recall, melhor equilíbrio com accuracy)
#   - Grid Search de hiperparâmetros
#   - Split 80/20 ALEATÓRIO (não espacial)
#   - SMOTENC 50%, SEM normalização prévia
#     (o artigo não normaliza em nenhum momento do pipeline de classificação)
#   - Matriz de confusão no teste (Fig. 4a) e na base completa (Fig. 4b)
#   - SHAP via fastshap + shapviz (aproximação de Monte Carlo do
#     shap.TreeExplainer usado no artigo em Python — mesma base teórica,
#     implementação diferente)
#   - Clusterização k-means (k=3) sobre hexágonos classificados como favela
# ==============================================================================

# ==============================================================================
# 1. PACOTES
# ==============================================================================
gc()

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))


# ==============================================================================
# 2. IMPORTAÇÃO — BASE + ARTEFATO DE SENSIBILIDADE (HERDA O LIMIAR)
# ==============================================================================
pasta_entrada <- here("data", "processed")
pasta_saida <- here("outputs", "artefato")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# a resolução está fixada para rodar esse
# script individualmente para cada resolução (9, 10 ou 11).
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 10

# opçoes: area_pura ou ponderado_edificações
# Defina a opção desejada: "area_pura" ou "ponderado_edificacoes"
tipo_grid <- "area_pura" # ou "ponderado_edificacoes"

# SENSIBILIDADE ------------------------------------------------------------------
sufixo_saida <- paste0("_res", resolucao_hex)

# caminho alinhado à convenção real de saída do 01a/01b
# (outputs/artefato/dt/{tipo_grid}/sensibilidade_artigo_res{N}.rds)
caminho_sensibilidade <- file.path(
  here::here("outputs", "artefato", "dt", tipo_grid),
  paste0("sensibilidade_artigo", sufixo_saida, ".rds")
)

if (!file.exists(caminho_sensibilidade)) {
  stop(
    "Artefato de sensibilidade não encontrado em ",
    caminho_sensibilidade,
    ". ",
    "Rode 01a_article_sensibility_analysis_dt.R primeiro (com a ",
    "mesma 'resolucao_hex' e 'tipo_grid' definidas aqui) — este script ",
    "depende do limiar de cobertura decidido lá."
  )
}

artefato_sensibilidade <- readRDS(caminho_sensibilidade)

names(artefato_sensibilidade)
artefato_sensibilidade$limiar_escolhido
artefato_sensibilidade$comparacao_limiares

# LIMIAR
# O 'limiar_escolhido' é o 70% fixo do artigo mantido como referência).
limiar_id <- artefato_sensibilidade$comparacao_limiares$Threshold[1]

limiar_pct <- as.numeric(str_extract(as.character(limiar_id), "\\d+"))
limiar_fracao <- limiar_pct / 100
limiar_fracao

message(sprintf(
  ">>> Limiar de cobertura espacial herdado da sensibilidade: %d%% (id: %s)",
  limiar_pct,
  limiar_id
))


# GRID HEXAGONAL -------------------------------------------------------------------
# opçoes: area_pura ou ponderado_edificações
caminho_grid_hexagonal <- file.path(
  pasta_entrada,
  paste0("grid_hexagonal_", tipo_grid, sufixo_saida, ".rds")
)

# importar o artigo do grid hexagonal, já definido se é area_Pura ou ponderado
grid_hexagonal <-
  readRDS(caminho_grid_hexagonal)

# visualizar o data.frame
glimpse(grid_hexagonal)

# ajustes finais na base
dados_base <-
  grid_hexagonal |>
  st_transform(31983) |>
  select(
    id_hex,
    geometry,
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
  mutate(across(where(is.logical), as.numeric)) |>
  mutate(
    is_slum = factor(
      if_else(slum_coverage_pct >= limiar_fracao, "Slum", "Non_Slum"),
      levels = c("Slum", "Non_Slum")
    )
  )

# ==============================================================================
# 3. SPLIT 80/20 — ALEATÓRIO (Seção 3.5: "randomly split", não espacial)
# ==============================================================================
set.seed(42)
split_inicial <-
  initial_split(
    dados_base |>
      st_drop_geometry(),
    prop = 0.80,
    strata = is_slum
  )

dados_treino <- training(split_inicial)
dados_teste <- testing(split_inicial)

# ==============================================================================
# 4. RECEITA DE PRÉ-PROCESSAMENTO
# ==============================================================================
# OMISSÃO DE PROPÓSITO: sem step_normalize().
#
# O artigo não normaliza em nenhum momento do pipeline de classificação
# (só cita padronização de CRS espacial, que é outra coisa).
# Isso é mantido aqui para fidelidade — a linha
# Melhoria (modelo_artigo_melhorado.R / modelo_artigo_melhorado_espacial.R)
# já resolve essa lacuna técnica.
#
# TEORIA — por que SMOTENC (e não SMOTE simples): as variáveis de entrada
# misturam contínuas (ex.: avg_slope_topodata) e categóricas/lógicas (ex.:
# highway_intersection).
#
# SMOTE puro assume espaço totalmente numérico;
# SMOTENC (Chawla et al., 2002, extensão para dados mistos) preserva a natureza
# categórica ao gerar sintéticos, evitando "meias-categorias" sem sentido.
receita <-
  recipe(is_slum ~ ., data = dados_treino) |>
  update_role(id_hex, new_role = "id") |>
  step_rm(slum_coverage_pct) |>
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.90, method = "spearman") |>
  # balanceamento com fatores
  step_smotenc(is_slum, over_ratio = 0.5) |>
  # converte fatores em 0/1
  step_dummy(all_nominal_predictors())

# ==============================================================================
# 5. COMPARAÇÃO DE ALGORITMOS (reproduz a Tabela B.5 / Apêndice B)
# ==============================================================================
# O artigo compara 6 colunas na Tabela B.5
# (Decision tree, Random forest, Gradient boosting, AdaBoost, XGBoost, KNN),
# embora o texto do Apêndice B diga "seven different models" —
# inconsistência do próprio artigo entre texto e tabela, mantida aqui como
# está (6 modelos, os que aparecem na tabela).
# "Gradient boosting" e "AdaBoost" não têm engine nativa 1:1 no tidymodels;
# aproximamos por LightGBM (gradient boosting genérico) e C5.0 com boosting
# (mecanismo de reponderação por erro, no espírito do AdaBoost).

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

wf_set_comparacao <-
  workflow_set(
    preproc = list(receita = receita),
    models = especificacoes_modelos
  )

# "During cross-validation, the entire dataset was resampled over 10
# iterations, using 90% for training and 10% for validation" — o texto do
# artigo é ambíguo sobre se isso é a base inteira ou só o treino (o mesmo
# problema de redação que já identificamos no Apêndice A). Seguindo a leitura
# literal ("the entire dataset"), a CV de seleção de algoritmo aqui roda sobre
# a base COMPLETA — a avaliação honesta do modelo final (Seções 8-9) usa,
# separadamente, o holdout de 20% nunca tocado por essa CV.
set.seed(123)
folds_comparacao <-
  vfold_cv(
    dados_base |>
      st_drop_geometry(),
    v = 10,
    strata = is_slum
  )

# quantidade de sessões utilizadas
# future::plan(multisession, workers = 3)
future::plan(sequential)

resultados_comparacao <-
  workflow_map(
    wf_set_comparacao,
    fn = "fit_resamples",
    resamples = folds_comparacao,
    metrics = metric_set(accuracy, precision, recall, roc_auc, f_meas),
    control = control_resamples(save_pred = TRUE)
  )

resultados_comparacao
# Isso vai imprimir o erro exato, se for necessário
# resultados_comparacao$result[[1]]

tabela_b5_reconstruida <-
  collect_metrics(resultados_comparacao) |>
  select(wflow_id, .metric, mean) |>
  # Transforma cada modelo (wflow_id) em uma coluna separada
  pivot_wider(names_from = wflow_id, values_from = mean) |>
  # Ordena as linhas (métricas) na mesma ordem da imagem (Accuracy, Precision, Recall, Roc auc, f_meas)
  slice(
    match(
      c("accuracy", "precision", "recall", "roc_auc", "f_meas"),
      .metric
    )
  )
tabela_b5_reconstruida


# ==============================================================================
# 6. SELEÇÃO DO MODELO
# (artigo: "Random Forest... optimal balance between accuracy and recall")
# ==============================================================================
vencedor_id <-
  tabela_b5_reconstruida |>
  # Filtra apenas a linha correspondente ao "recall"
  filter(.data[[coluna_metrica]] == "recall") |>
  # Remove a coluna de métricas, restando apenas os valores numéricos dos modelos
  select(-all_of(coluna_metrica)) |>
  # Pivoteia temporariamente para encontrar o maior valor de forma simples
  pivot_longer(everything(), names_to = "wflow_id", values_to = "mean") |>
  slice_max(mean, n = 1) |>
  pull(wflow_id)
vencedor_id

if (!str_detect(vencedor_id, "random_forest")) {
  message(
    ">>> ATENÇÃO: o algoritmo de maior recall nesta execução (",
    vencedor_id,
    ") não foi Random Forest, diferente do artigo. Prosseguindo com Random ",
    "Forest mesmo assim, para manter fidelidade à Seção 4.2 do artigo — ",
    "considere investigar essa divergência."
  )
}

spec_final_base <-
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

wf_final_base <-
  workflow() |>
  add_recipe(receita) |>
  add_model(spec_final_base)

# ==============================================================================
# 7. GRID SEARCH
# (Seção 3.5: "a Grid Search was conducted to optimize its parameters")
# ==============================================================================
grade_hiperparametros <-
  grid_regular(
    mtry(range = c(2, 12)),
    trees(range = c(300, 1000)),
    min_n(range = c(2, 20)),
    levels = 3
  )

set.seed(456)

folds_grid_search <-
  vfold_cv(
    dados_treino,
    v = 10,
    strata = is_slum
  )

#future::plan(multisession, workers = max(1, availableCores() - 1))
future::plan(sequential)

# save_pred = TRUE é essencial aqui:
# é dessas predições out-of-fold
# (cada hexágono de treino previsto por uma árvore que NÃO o viu)
# que vamos extrair
# o limiar de decisão ótimo na Seção 8 — sem contaminar o holdout de teste.
resultado_grid_search <-
  tune_grid(
    wf_final_base,
    resamples = folds_grid_search,
    grid = grade_hiperparametros,
    metrics = metric_set(accuracy, precision, recall, roc_auc, f_meas),
    control = control_grid(save_pred = TRUE)
  )
resultado_grid_search

melhores_hiperparametros <-
  select_best(
    resultado_grid_search,
    metric = "recall"
  )

# Melhores hiperparâmetros
# (Grid Search, critério: recall)
print(melhores_hiperparametros)

wf_final <-
  finalize_workflow(
    wf_final_base,
    melhores_hiperparametros
  )

# ==============================================================================
# 8. AJUSTE FINAL + LIMIAR DE DECISÃO (SEM VAZAMENTO) [B6][B11]
# ==============================================================================
# LIMIAR DE DECISÃO: a Seção 3.6 do artigo afirma que as medições
# finais ("Fig. 4a/4b") foram feitas "with the 70% probability threshold
# from the model results, based in the results of the sensitivity analysis
# in Section 3.3" — ou seja, o MESMO número usado para definir o rótulo
# (cobertura de favela ≥ 70%) é reaproveitado como limiar sobre a
# probabilidade prevista pelo modelo. Para máxima fidelidade ao texto,
# LIMIAR_PADRAO é definido como limiar_fracao (herdado da sensibilidade,
# Seção 2 deste script) — não mais um valor fixo de 0.5. A convenção
# genérica de 0.5 continua disponível só como referência visual no gráfico
# de perfil abaixo, não como resultado reportado.
LIMIAR_PADRAO <-
  limiar_fracao

# [CORREÇÃO] 'LIMIAR_ARTIGO' era referenciado na exportação final (Seção
# 14) mas nunca era definido em lugar nenhum do script — quebraria com
# "object 'LIMIAR_ARTIGO' not found" só no fim, depois de já ter pago o
# custo inteiro do Grid Search e do SHAP (mesma classe de bug que os
# itens B1-B3 do changelog já corrigiram para outras variáveis).
# Semanticamente, 'LIMIAR_ARTIGO' é o mesmo valor de 'LIMIAR_PADRAO' aqui
# (ambos = limiar_fracao, ~70%, fiel ao texto do artigo) — mantidos como
# constantes separadas porque a exportação final trata como campos
# distintos (um é o "resultado reportado", o outro a "referência
# textual"), mas ambos herdam do mesmo limiar_fracao.
LIMIAR_ARTIGO <-
  limiar_fracao

fit_final <-
  fit(
    wf_final,
    data = dados_treino
  )

# --- TEORIA: por que otimizar o limiar sobre OOF, não sobre o teste ---
# Se escolhermos o limiar de decisão observando o desempenho no próprio
# conjunto de teste, e DEPOIS reportamos o desempenho nesse mesmo teste como
# "resultado honesto", estamos correndo o mesmo risco que Cawley & Talbot
# (2010, "On Over-fitting in Model Selection") descrevem: o PROCESSO de
# escolha (não só o modelo final) precisa ser validado fora da amostra usada
# para o veredito final. A solução aqui é usar as predições OUT-OF-FOLD do
# próprio Grid Search (Seção 7) — cada hexágono de treino previsto por uma
# reamostra que não o usou para treinar — como "conjunto de validação"
# substituto. O teste (20%) permanece intocado até a Seção 9.
#
# NOTA DE FIDELIDADE: o limiar abaixo (J de Youden, sobre OOF) é uma
# ANÁLISE COMPLEMENTAR, não mencionada no artigo — o texto não nomeia
# nenhuma estatística para escolher o ponto de corte, só afirma que 70% foi
# usado. Ele é mantido como diagnóstico de robustez ("será que 70% é mesmo
# uma boa escolha de limiar de decisão, independentemente de já ser o
# limiar de cobertura do rótulo?"), mas não substitui LIMIAR_PADRAO como
# resultado principal deste script.
preds_out_of_fold_treino <-
  collect_predictions(
    resultado_grid_search,
    parameters = melhores_hiperparametros
  )


avaliacao_limiares <-
  probably::threshold_perf(
    preds_out_of_fold_treino,
    truth = is_slum,
    estimate = .pred_Slum,
    thresholds = seq(0.10, 0.70, by = 0.10),
    metrics = metric_set(
      j_index,
      f_meas,
      accuracy,
      sens,
      yardstick::spec, # Correção feita aqui
      precision
    ),
    event_level = "first"
  )

unique(avaliacao_limiares$.metric)

# TEORIA — J de Youden (j_index = Sensibilidade + Especificidade − 1): mede o
# quanto o classificador se afasta de um "chute aleatório" (J=0) em direção a
# um classificador perfeito (J=1), balanceando os dois tipos de erro sem
# assumir nenhum custo relativo entre eles a priori — é o critério mais citado
# na literatura de escolha de ponto de corte quando não há uma razão de custo
# explícita entre falso positivo e falso negativo (Youden, 1950).
melhor_limiar_obj <-
  avaliacao_limiares |>
  filter(.metric == "j_index") |>
  slice_max(.estimate, n = 1, with_ties = FALSE)

LIMIAR_OTIMIZADO <-
  melhor_limiar_obj$.threshold[1]
# diagnóstico complementar, ver nota acima

message(sprintf(
  ">>> Limiar principal (fiel ao artigo, Seção 3.6): %.2f 
  (= limiar de cobertura herdado da sensibilidade)",
  LIMIAR_PADRAO
))

message(sprintf(
  ">>> Limiar otimizado (J de Youden, predições OOF do Grid Search — DIAGNÓSTICO
     COMPLEMENTAR, não faz parte do artigo): %.2f (J = %.4f)",
  LIMIAR_OTIMIZADO,
  melhor_limiar_obj$.estimate[1]
))

if (abs(LIMIAR_OTIMIZADO - LIMIAR_PADRAO) < 0.05) {
  message(
    ">>> O limiar estatisticamente ótimo (J de Youden) está próximo do limiar ",
    "literal do artigo (70%) — evidência de que a escolha do artigo é razoável ",
    "também sob esse critério alternativo, não só por citação direta do texto."
  )
}


# Gráfico do perfil de limiares — útil para inspecionar se o pico do J-index é
# bem definido ou se há um platô largo (indicando pouca sensibilidade ao ponto
# de corte exato). A linha pontilhada em 0.5 é só referência visual da
# convenção genérica de ML — não é usada em nenhum cálculo deste script.
grafico_perfil_limiares_all <-
  avaliacao_limiares |>
  filter(.metric %in% c("j_index", "sens", "spec", "precision")) |>
  mutate(.metric = ifelse(.metric == "sens", "recall", .metric)) |>
  ggplot(aes(x = .threshold, y = .estimate, color = .metric)) +
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = LIMIAR_OTIMIZADO,
    linetype = "dashed",
    color = "firebrick"
  ) +
  geom_vline(xintercept = LIMIAR_PADRAO, linetype = "solid", color = "gray20") +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray60") +
  scale_color_manual(
    values = c(
      "j_index" = "#D55E00", # Vermelho-alaranjado
      "recall" = "#0072B2", # Azul forte
      "spec" = "yellow", # Verde-azulado
      "precision" = "black" # black
    ),
    labels = c(
      # ⬅️ AGORA COM NOMES para parear corretamente
      "j_index" = "Índice de Youden",
      "recall" = "Recall",
      "spec" = "Especificidade",
      "precision" = "Precisão"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent
  ) +
  annotate(
    "text",
    x = LIMIAR_OTIMIZADO,
    y = 0,
    label = "J de Youden",
    hjust = -0.1,
    color = "firebrick",
    size = 6
  ) +
  annotate(
    "text",
    x = LIMIAR_PADRAO,
    y = 0.05,
    label = "limiar do artigo",
    hjust = 1.1,
    color = "gray20",
    size = 6
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0,
    label = "convenção 0.5",
    hjust = 0.6,
    color = "gray60",
    size = 6
  ) +
  labs(
    title = "Perfil de limiares de decisão (predições out-of-fold do grid search)",
    subtitle = "Linha sólida = limiar 70% | Linha tracejada = limiar 20% (J de Youden)",
    x = "Limiar de Probabilidade do rótulo",
    y = "Desempenho médio (100-fold CV)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(
      size = 16,
      color = "gray30",
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 16, margin = margin(t = 8)),
    axis.title.y = element_text(size = 16, margin = margin(r = 8)),
    # Eixo X configurado com angle = 0 para manter o texto totalmente na horizontal
    axis.text.x = element_text(
      size = 14,
      color = "black",
      angle = 0,
      hjust = 0.5
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = -5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    panel.border = element_rect(color = "black", linewidth = 0.8)
  )
print(grafico_perfil_limiares_all)


# Gráfico do perfil de limiares (Apenas Precisão e Recall) com padrão SBSR
grafico_perfil_limiares <-
  avaliacao_limiares |>
  # 1. Filtra para manter APENAS sensibilidade (recall) e precisão
  filter(.metric %in% c("sens", "precision")) |>
  mutate(.metric = ifelse(.metric == "sens", "recall", .metric)) |>
  ggplot(aes(x = .threshold, y = .estimate, color = .metric)) +

  # 2. Linhas verticais de destaque (Limiares do Modelo)
  geom_vline(
    xintercept = LIMIAR_OTIMIZADO,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = LIMIAR_PADRAO,
    linetype = "solid",
    color = "gray20",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = 0.5,
    linetype = "dotted",
    color = "gray60",
    linewidth = 0.8
  ) +

  # 3. Séries de dados (Linhas e Pontos)
  geom_line(linewidth = 1.2) +
  # Filtra os dados SOMENTE para os pontos, plotando a cada 0.05 (5%) para ficarem mais separados
  geom_point(
    data = function(d) subset(d, round(.threshold * 100, 0) %% 5 == 0),
    size = 2.5
  ) +

  # 4. Escalas dos eixos
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +

  # 5. Cores com contraste acessível e ordem forçada na legenda (SBSR)
  scale_color_manual(
    values = c("recall" = "#2b83ba", "precision" = "#d7191c"),
    labels = c("precision" = "Precisão", "recall" = "Recall"),
    breaks = c("precision", "recall")
  ) +

  # 6. Anotações textuais
  annotate(
    "text",
    x = LIMIAR_OTIMIZADO,
    y = 0.02,
    label = "J de Youden",
    hjust = -0.1,
    color = "firebrick",
    size = 6,
    family = "serif",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = LIMIAR_PADRAO,
    y = 0.07,
    label = "Limiar do artigo",
    hjust = -0.1,
    color = "gray20",
    size = 6,
    family = "serif",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.02,
    label = "Convenção 0,5",
    hjust = -0.1,
    color = "gray60",
    size = 6,
    family = "serif",
    fontface = "italic"
  ) +

  # 7. Rótulos
  labs(
    x = "Limiar de Probabilidade do rótulo",
    y = "Desempenho médio (100-fold CV)",
    color = NULL
  ) +

  # 8. Tema visual acadêmico aprimorado
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(
      size = 16,
      color = "gray30",
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 16, margin = margin(t = 8)),
    axis.title.y = element_text(size = 16, margin = margin(r = 8)),
    # Eixo X configurado com angle = 0 para manter o texto totalmente na horizontal
    axis.text.x = element_text(
      size = 14,
      color = "black",
      angle = 0,
      hjust = 0.5
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = -5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    panel.border = element_rect(color = "black", linewidth = 0.8)
  )
print(grafico_perfil_limiares)


# ------------------------------------------------------------------------------
# GERAÇÃO DAS COLUNAS DE DECISÃO (TREINO, TESTE E COMPLETO)
# ------------------------------------------------------------------------------
gerar_predicoes <- function(dados_alvo, incluir_id = FALSE) {
  cols_extra <- if (incluir_id) c("id_hex", "is_slum") else "is_slum"
  predict(fit_final, dados_alvo, type = "prob") |>
    bind_cols(dados_alvo |> select(all_of(cols_extra))) |>
    mutate(
      classe_decisao_70 = factor(
        if_else(.pred_Slum >= LIMIAR_PADRAO, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      ),
      classe_decisao_20 = factor(
        if_else(.pred_Slum >= LIMIAR_OTIMIZADO, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      )
    )
}

preds_treino <- gerar_predicoes(dados_treino)
preds_teste <- gerar_predicoes(dados_teste)

preds_completo <- gerar_predicoes(
  dados_base |> st_drop_geometry(),
  incluir_id = TRUE
)
preds_completo

# ==============================================================================
# 9. MATRIZES DE CONFUSÃO (Fig. 4a e 4b) [B1] — nunca eram criadas no v2
# ==============================================================================
matriz_confusao_treino_70 <- conf_mat(
  preds_treino,
  truth = is_slum,
  estimate = classe_decisao_70
)

matriz_confusao_treino_20 <- conf_mat(
  preds_treino,
  truth = is_slum,
  estimate = classe_decisao_20
)

matriz_confusao_teste_70 <- conf_mat(
  preds_teste,
  truth = is_slum,
  estimate = classe_decisao_70
)

matriz_confusao_teste_20 <- conf_mat(
  preds_teste,
  truth = is_slum,
  estimate = classe_decisao_20
)

matriz_confusao_completa_70 <- conf_mat(
  preds_completo,
  truth = is_slum,
  estimate = classe_decisao_70
)

matriz_confusao_completa_20 <- conf_mat(
  preds_completo,
  truth = is_slum,
  estimate = classe_decisao_20
)

# Mantém os nomes usados pelo v2 no artefato final (retrocompatibilidade)
matriz_confusao_teste_70 <- matriz_confusao_teste_70
matriz_confusao_completa_70 <- matriz_confusao_completa_70

matriz_confusao_teste_70
matriz_confusao_teste_20
matriz_confusao_completa_70
matriz_confusao_completa_20

# Sua função preparar_dados_cm (já está correta)
preparar_dados_cm <- function(matriz) {
  if (inherits(matriz, "conf_mat")) {
    m <- as.matrix(matriz$table)
  } else {
    m <- as.matrix(matriz)
  }
  if (!all(dim(m) == c(2, 2))) {
    stop("A matriz deve ser 2x2")
  }
  df <- data.frame(
    True_label = factor(c(1, 0, 1, 0), levels = c(0, 1)),
    Predicted_label = factor(c(1, 1, 0, 0), levels = c(0, 1)),
    Freq = c(
      m[1, 1], # TP
      m[1, 2], # FP
      m[2, 1], # FN
      m[2, 2] # TN
    )
  )
  max_val <- max(df$Freq)
  df$Text_Color <- ifelse(df$Freq > (max_val * 0.4), "black", "yellow")
  return(df)
}

# Preparar os dados
matriz_confusao_teste_70 <- preparar_dados_cm(matriz_confusao_teste_70)
matriz_confusao_teste_20 <- preparar_dados_cm(matriz_confusao_teste_20)
matriz_confusao_completa_70 <- preparar_dados_cm(matriz_confusao_completa_70)
matriz_confusao_completa_20 <- preparar_dados_cm(matriz_confusao_completa_20)

# Função para plotar (com legenda ajustada)
plotar_cm <- function(df, titulo) {
  ggplot(df, aes(x = Predicted_label, y = True_label, fill = Freq)) +
    geom_tile(color = "black", linewidth = 0.8) +
    geom_text(
      aes(label = Freq, color = Text_Color),
      size = 12,
      fontface = "bold"
    ) +
    scale_color_identity() +
    scale_fill_viridis_c(
      option = "viridis",
      direction = 1,
      guide = "none"
    ) +
    scale_y_discrete(limits = c("1", "0")) +
    scale_x_discrete(limits = c("0", "1")) +
    coord_fixed() +
    labs(
      title = titulo,
      x = "Predicted label",
      y = "True label"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.position = "right",
      legend.margin = margin(l = 10)
    )
}

# Gerar os gráficos
p1 <- plotar_cm(
  matriz_confusao_teste_70,
  "Confusion Matrix with test data (thresholds 70%)"
)
p2 <- plotar_cm(
  matriz_confusao_teste_20,
  "Confusion Matrix with test data (thresholds 20%)"
)
p3 <- plotar_cm(
  matriz_confusao_completa_70,
  "Confusion Matrix with all data (thresholds 70%)"
)
p4 <- plotar_cm(
  matriz_confusao_completa_20,
  "Confusion Matrix with all data (thresholds 20%)"
)

# Combinar com patchwork (lado a lado)
figure_matriz_confusion <-
  p1 + p2 + p3 + p4
print(figure_matriz_confusion)

# Função para plotar uma matriz de confusão
plotar_cm <- function(df, titulo, mostrar_barra = TRUE) {
  ggplot(df, aes(x = Predicted_label, y = True_label, fill = Freq)) +
    geom_tile(color = "black", linewidth = 0.8) +
    geom_text(
      aes(
        label = number(Freq, big.mark = ".", decimal.mark = ",", accuracy = 1),
        color = Text_Color
      ),
      size = 10,
      fontface = "bold",
      family = "serif"
    ) +
    scale_color_identity() +
    scale_fill_viridis_c(
      option = "viridis",
      direction = 1,
      guide = "none"
    ) +
    scale_x_discrete(limits = c("0", "1"), labels = c("Não favela", "Favela")) +
    scale_y_discrete(limits = c("1", "0"), labels = c("Favela", "Não favela")) +
    coord_fixed() +
    labs(
      title = titulo,
      x = "Classe predita",
      y = "Classe real"
    ) +
    theme_minimal(base_size = 14, base_family = "serif") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 14),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      panel.grid = element_blank(),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.position = "right",
      legend.margin = margin(l = 10)
    )
}

# Gráficos individuais
p1 <- plotar_cm(matriz_confusao_teste_70, "Teste, limiar de 70%")
p2 <- plotar_cm(matriz_confusao_teste_20, "Teste, limiar de 20% (J de Youden)")
p3 <- plotar_cm(matriz_confusao_completa_70, "Base completa, limiar de 70%")
p4 <- plotar_cm(
  matriz_confusao_completa_20,
  "Base completa, limiar de 20% (J de Youden)"
)

# Combinação 2 x 2 com identificação (a) a (d)
figure_matriz_confusion_article <-
  wrap_plots(p1, p2, p3, p4, ncol = 2) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(plot.tag = element_text(face = "bold", family = "serif", size = 14))
print(figure_matriz_confusion_artticle)


# ------------------------------------------------------------------------------
# MÉTRICAS — TREINO / TESTE / COMPLETO, PADRÃO E OTIMIZADO
# ------------------------------------------------------------------------------
calcular_metricas <- function(preds, coluna_estimate, cenario) {
  tibble(
    Cenario = cenario,
    Recall = recall(
      preds,
      truth = is_slum,
      estimate = {{ coluna_estimate }},
      event_level = "first"
    )$.estimate,
    Accuracy = accuracy(
      preds,
      truth = is_slum,
      estimate = {{ coluna_estimate }}
    )$.estimate,
    Precision = precision(
      preds,
      truth = is_slum,
      estimate = {{ coluna_estimate }}
    )$.estimate,
    F1_Score = f_meas(
      preds,
      truth = is_slum,
      estimate = {{ coluna_estimate }}
    )$.estimate
  )
}

#Agora: uma função só,
# reaproveitada para os 3 cenários x 2 limiares, e o treino entra na tabela —
# é o diagnóstico de overfitting que o próprio artigo usa (Seção 4.2).

# treino
metricas_treino_70 <- calcular_metricas(
  preds_treino,
  classe_decisao_70,
  "Treino (limiar 70%)"
)
metricas_treino_70

metricas_treino_20 <- calcular_metricas(
  preds_treino,
  classe_decisao_20,
  "Treino (limiar 20%)"
)
metricas_treino_20

# teste
metricas_teste_70 <- calcular_metricas(
  preds_teste,
  classe_decisao_70,
  "Teste (limiar 70%)"
)
metricas_teste_70

metricas_teste_20 <- calcular_metricas(
  preds_teste,
  classe_decisao_20,
  "Teste (limiar 20%)"
)
metricas_teste_20

metricas_completo_70 <- calcular_metricas(
  preds_completo,
  classe_decisao_70,
  "Completo (limiar 70%)"
)
metricas_completo_70

metricas_completo_20 <- calcular_metricas(
  preds_completo,
  classe_decisao_20,
  "Completo (limiar 20%)"
)
metricas_completo_20


# Valores extraídos do artigo original —
# TESTE (Fig. 4a): TP=421, FN=66, TN=9359, FP=1125
recall_artigo_teste <- 0.8645
accuracy_artigo_teste <- (9359 + 421) / (9359 + 1125 + 66 + 421)
precision_artigo_teste <- 421 / (421 + 1125)

f_meas_artigo_teste <- 2 *
  (precision_artigo_teste * recall_artigo_teste) /
  (precision_artigo_teste + recall_artigo_teste)

# Valores extraídos do artigo original —
# BASE COMPLETA (Fig. 4b): TP=2059, FN=345, TN=46881, FP=5570
recall_artigo_completo <- 2059 / (2059 + 345)
accuracy_artigo_completo <- (46881 + 2059) / (46881 + 5570 + 345 + 2059)
precision_artigo_completo <- 2059 / (2059 + 5570)

f_meas_artigo_completo <- 2 *
  (precision_artigo_completo * recall_artigo_completo) /
  (precision_artigo_completo + recall_artigo_completo)

linha_artigo_teste <- tibble(
  Cenario = "Artigo original (Fig. 4a)",
  Recall = recall_artigo_teste,
  Accuracy = accuracy_artigo_teste,
  Precision = precision_artigo_teste,
  F1_Score = f_meas_artigo_teste
)

linha_artigo_completo <- tibble(
  Cenario = "Artigo original (Fig. 4b)",
  Recall = recall_artigo_completo,
  Accuracy = accuracy_artigo_completo,
  Precision = precision_artigo_completo,
  F1_Score = f_meas_artigo_completo
)

# --- Tabela 1: diagnóstico Treino -> Teste -> Completo (generalização) ---
tabela_generalizacao <- bind_rows(
  metricas_treino_70,
  metricas_teste_70,
  metricas_completo_70
)


# --- Tabela 2: teste, padrão vs. otimizado vs. artigo ---
tabela_comparativa_teste <- bind_rows(
  metricas_teste_70,
  metricas_teste_20,
  linha_artigo_teste
)
tabela_comparativa_teste

# --- Tabela 3: base completa, padrão vs. otimizado vs. artigo ---
tabela_comparativa_completa <- bind_rows(
  metricas_completo_70,
  metricas_completo_20,
  linha_artigo_completo
)
tabela_comparativa_completa


# tabelas reais, com % formatado e a
# melhor linha de recall destacada — não dependem do console mostrar message().
construir_gt_comparativo <- function(tabela, titulo, subtitulo) {
  tabela |>
    gt() |>
    tab_header(title = titulo, subtitle = subtitulo) |>
    fmt_percent(
      columns = c(Recall, Accuracy, Precision, F1_Score),
      decimals = 2
    ) |>
    data_color(
      columns = Recall,
      colors = scales::col_numeric(
        palette = c("white", "#2c7fb8"),
        domain = c(0, 1)
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = Recall == max(Recall))
    )
}

gt_generalizacao <- construir_gt_comparativo(
  tabela_generalizacao,
  "Diagnóstico de generalização (equivalente à Seção 4.2 do artigo)",
  "Queda de recall Treino -> Teste é esperada; queda abrupta demais sugere overfitting"
)
gt_generalizacao

gt_teste <- construir_gt_comparativo(
  tabela_comparativa_teste,
  "Avaliação no conjunto de teste (holdout 20%)",
  "Esta é a avaliação mais honesta do modelo — simula dados nunca vistos"
)
gt_teste

gt_completo <- construir_gt_comparativo(
  tabela_comparativa_completa,
  "Avaliação na base completa (treino + teste)",
  "Contém viés de otimismo (80% dos dados já 'vistos' pelo modelo) — usada no artigo para a superfície probabilística"
)
gt_completo


# ==============================================================================
# 10. SUPERFÍCIE DE PROBABILIDADE — BINS (Tabela 2) E MAPA (Fig. 5)
# ==============================================================================
cortes_referencia <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

superficie_probabilidade <-
  dados_base |>
  select(id_hex, geometry, is_slum) |>
  inner_join(
    preds_completo |>
      select(id_hex, .pred_Slum, classe_decisao_70, classe_decisao_20),
    by = "id_hex"
  ) |>
  mutate(
    # Estes bins são a Tabela 2 do artigo (Seção 4.3): cinco faixas
    # FIXAS de 50% a 100%, usadas na avaliação qualitativa por amostragem —
    # não dependem do valor de LIMIAR_PADRAO, que é outra coisa (o corte de
    # decisão Slum/Non_Slum sobre a probabilidade, usado nas Figs. 4/6).
    # Os cortes continuam fixos aqui de propósito, mesmo após [B11].
    bin_probabilidade_padrao = cut(
      .pred_Slum,
      breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
      labels = c("50-59", "60-69", "70-79", "80-89", "90-100"),
      include.lowest = TRUE,
      right = FALSE
    ),
    bin_probabilidade_20 = cut(
      .pred_Slum,
      breaks = unique(c(
        LIMIAR_OTIMIZADO,
        cortes_referencia[cortes_referencia > LIMIAR_OTIMIZADO]
      )),
      include.lowest = TRUE,
      right = FALSE
    )
  )
superficie_probabilidade


bins_padrao <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_padrao)) |>
  count(bin_probabilidade_padrao, name = "n_hexagonos")
bins_padrao

gt_bins_padrao <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_padrao)) |>
  count(bin_probabilidade_padrao, name = "n_hexagonos") |>
  gt() |>
  tab_header(
    title = "Distribuição por bin de probabilidade — limiar padrão (>= 50%)"
  )
gt_bins_padrao


bins_20 <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_20)) |>
  count(bin_probabilidade_20, name = "n_hexagonos")
bins_20

gt_bins_20 <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_20)) |>
  count(bin_probabilidade_20, name = "n_hexagonos") |>
  gt() |>
  tab_header(
    title = sprintf(
      "Distribuição por bin de probabilidade — limiar otimizado (>= %.2f)",
      LIMIAR_OTIMIZADO
    )
  )
gt_bins_20


# ==============================================================================
# 11. MAPA DE FALSOS POSITIVOS (Fig. 6)
# ==============================================================================
falsos_positivos_70 <-
  superficie_probabilidade |>
  filter(classe_decisao_70 == "Slum", is_slum == "Non_Slum")

falsos_positivos_20 <-
  superficie_probabilidade |>
  filter(classe_decisao_20 == "Slum", is_slum == "Non_Slum")

tabela_falsos_positivos <- tibble(
  Cenario = c(
    sprintf("Limiar do artigo (%.0f%%)", LIMIAR_PADRAO * 100),
    sprintf("Limiar otimizado, diagnóstico (%.2f)", LIMIAR_OTIMIZADO)
  ),
  N_Falsos_Positivos = c(
    nrow(falsos_positivos_70),
    nrow(falsos_positivos_20)
  ),
  Pct_Total_Hexagonos = c(
    nrow(falsos_positivos_70) / nrow(superficie_probabilidade),
    nrow(falsos_positivos_20) / nrow(superficie_probabilidade)
  )
)
tabela_falsos_positivos

gt_falsos_positivos <-
  tabela_falsos_positivos |>
  gt() |>
  tab_header(
    title = "Falsos positivos identificados na base completa (Fig. 6)"
  ) |>
  fmt_percent(columns = Pct_Total_Hexagonos, decimals = 2) |>
  fmt_number(columns = N_Falsos_Positivos, decimals = 0)

print(gt_falsos_positivos)

# ==============================================================================
# 12. SHAP — IMPORTÂNCIA DE FEATURES (Fig. 9) — reforçado
# ==============================================================================
# fastshap aproxima por Monte Carlo o que o artigo calculou com
# shap.TreeExplainer (Python, exato para modelos de árvore) — mesma base
# teórica (Lundberg & Lee, 2017), implementação diferente.
#
# TEORIA — o que muda nesta versão:
#   - Amostra maior (2000 em vez de 1000) e nsim maior (100 em vez de 50):
#     fastshap estima cada valor de Shapley por permutação Monte Carlo: quanto
#     maior nsim, menor a variância da estimativa (mas maior o custo
#     computacional — é um trade-off de precisão x tempo, não "mais é sempre
#     melhor" sem limite).
#   - Checagem de ADITIVIDADE: a propriedade central de Shapley values é que
#     a soma das contribuições de cada feature + o valor-base (previsão média)
#     deve reconstruir a previsão individual do modelo. Verificar isso é um
#     teste de sanidade barato que confirma que a aproximação está coerente.

receita_preparada <- extract_recipe(fit_final)
modelo_puro <- extract_fit_parsnip(fit_final)

set.seed(2024)
amostra_shap <- dados_teste |> slice_sample(n = min(2000, nrow(dados_teste)))

X_shap <- bake(receita_preparada, new_data = amostra_shap, all_predictors())

pred_wrapper_shap <- function(object, newdata) {
  predict(object, new_data = newdata, type = "prob")$.pred_Slum
}

set.seed(789)
valores_shap <- fastshap::explain(
  modelo_puro,
  X = X_shap,
  pred_wrapper = pred_wrapper_shap,
  nsim = 100
)

# --- Checagem de aditividade (sanidade) ---
baseline_shap <- mean(pred_wrapper_shap(modelo_puro, X_shap))
soma_shap_mais_baseline <- rowSums(as.matrix(valores_shap)) + baseline_shap
previsao_real <- pred_wrapper_shap(modelo_puro, X_shap)
erro_medio_aditividade <- mean(abs(soma_shap_mais_baseline - previsao_real))

message(sprintf(
  ">>> Checagem de aditividade SHAP: erro médio |soma(SHAP)+baseline - previsão| = %.5f (esperado próximo de 0)",
  erro_medio_aditividade
))

importancia_shap <-
  valores_shap |>
  as_tibble() |>
  summarise(across(everything(), ~ mean(abs(.x)))) |>
  pivot_longer(
    everything(),
    names_to = "feature",
    values_to = "shap_medio_abs"
  ) |>
  arrange(desc(shap_medio_abs))

shap_top10 <-
  importancia_shap |>
  slice_head(n = 10)

gt_shap_top10 <-
  importancia_shap |>
  slice_head(n = 10) |>
  gt() |>
  tab_header(
    title = "Top 10 features por |SHAP| médio (equivalente à Fig. 9)",
    subtitle = sprintf(
      "nsim = 100, amostra = %d hexágonos do teste",
      nrow(amostra_shap)
    )
  ) |>
  fmt_number(columns = shap_medio_abs, decimals = 4)
print(gt_shap_top10)

# --- Beeswarm real (direção + magnitude do efeito, como a Fig. 9) ---
objeto_shapviz <-
  shapviz(
    valores_shap,
    X = X_shap,
    baseline = baseline_shap
  )


grafico_shap_beeswarm <-
  sv_importance(
    objeto_shapviz,
    kind = "beeswarm",
    max_display = 10
  ) +
  labs(
    title = "Impacto das Variáveis no Modelo (SHAP Beeswarm)",
    x = "SHAP value (impact on model output)", # Padrão de artigo
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis_c(
    option = "viridis",
    direction = 1,
    name = "",
    labels = c("Low", "High"),
    breaks = c(0, 1),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = grid::unit(20, "cm"),
      barwidth = grid::unit(1.5, "cm")
    )
  )
grafico_shap_beeswarm


grafico_shap_barras <-
  sv_importance(
    objeto_shapviz,
    kind = "bar",
    max_display = 10
  ) +
  labs(
    title = "Impacto das Variáveis no Modelo (SHAP Beeswarm)",
    x = "SHAP value (impact on model output)", # Padrão de artigo
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis_c(
    option = "viridis",
    direction = 1,
    guide = guide_colorbar(
      title = NULL,
      barheight = grid::unit(20, "cm"),
      barwidth = grid::unit(1, "cm")
    )
  )
grafico_shap_barras


# ==============================================================================
# 13. CLUSTERIZAÇÃO K-MEANS (k=3) SOBRE HEXÁGONOS CLASSIFICADOS COMO FAVELA
# ==============================================================================
# Seção 3.8: variáveis de infraestrutura (água, esgoto), densidade (nº
# construções, área média, nº residências) e terreno (declividade). k=3 fixado
# por citação (Marques & Saraiva, 2017), validado no artigo via silhouette +
# elbow. [B11] A base de hexágonos usada agora é a decisão pelo LIMIAR DO
# ARTIGO (classe_decisao, 70%) — antes desta correção, a clusterização usava
# classe_decisao_otimizada (J de Youden), uma extensão que o artigo não
# menciona. Para fidelidade máxima, "áreas classificadas pelo modelo como
# favela" (o universo de entrada da Seção 3.8) deve refletir o mesmo limiar
# citado no texto, não um critério estatístico adicional só porque está
# disponível no script.
hexagonos_positivos <-
  superficie_probabilidade |>
  filter(classe_decisao_70 == "Slum") |>
  st_drop_geometry() |>
  inner_join(
    dados_base |>
      st_drop_geometry() |>
      select(
        id_hex,
        households_connected_to_water_supply_network,
        households_connected_to_sewerage_system,
        number_of_buildings,
        avg_building_area,
        number_private_residences,
        avg_slope_topodata
      ),
    by = "id_hex"
  )

variaveis_cluster <-
  hexagonos_positivos |>
  select(
    households_connected_to_water_supply_network,
    households_connected_to_sewerage_system,
    number_of_buildings,
    avg_building_area,
    number_private_residences,
    avg_slope_topodata
  ) |>
  drop_na()

# k-means é sensível a escala; o artigo não menciona padronização aqui, mas é
# uma necessidade técnica implícita (sem ela, number_of_buildings dominaria a
# distância euclidiana sozinho) — mesma lógica já aplicada ao SMOTE.
variaveis_cluster_escaladas <-
  scale(variaveis_cluster)

silhouette_por_k <- map_dfr(2:6, function(k) {
  set.seed(k)
  ajuste <- kmeans(variaveis_cluster_escaladas, centers = k, nstart = 25)
  sil <- silhouette(ajuste$cluster, dist(variaveis_cluster_escaladas))
  tibble(k = k, silhouette_media = mean(sil[, "sil_width"]))
})

grafico_silhouette <- ggplot(
  silhouette_por_k,
  aes(x = k, y = silhouette_media)
) +
  geom_segment(
    data = filter(silhouette_por_k, k == 2),
    aes(x = k, xend = k, y = 0, yend = silhouette_media),
    linetype = "dashed",
    color = "darkblue",
    linewidth = 0.8,
    alpha = 0.7
  ) +
  geom_segment(
    data = filter(silhouette_por_k, k == 3),
    aes(x = k, xend = k, y = 0, yend = silhouette_media),
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.8,
    alpha = 0.7
  ) +
  geom_line(color = "gray50", linewidth = 0.8) +
  geom_point(color = "gray40", size = 2.5) +
  geom_point(
    data = filter(silhouette_por_k, k == 2),
    aes(x = k, y = silhouette_media),
    color = "darkblue",
    size = 4.5,
    shape = 16
  ) +
  geom_point(
    data = filter(silhouette_por_k, k == 3),
    aes(x = k, y = silhouette_media),
    color = "firebrick",
    size = 5,
    shape = 8
  ) +
  scale_y_continuous(
    limits = c(0, max(silhouette_por_k$silhouette_media) * 1.15),
    breaks = seq(0, 0.4, by = 0.05)
  ) +
  scale_x_continuous(breaks = 2:6) +
  labs(
    title = "Validação do número de clusters (k-means)",
    subtitle = "Azul (k ótimo estatístico) | Vermelho (k=3, escolha metodológica do artigo)",
    x = "Número de clusters (k)",
    y = "Silhueta média (silhouette width)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )
print(grafico_silhouette)


melhor_k_estatistico <-
  silhouette_por_k |>
  slice_max(silhouette_media, n = 1, with_ties = FALSE) |>
  pull(k)
melhor_k_estatistico

melhor_k_artigo <- 3
melhor_k_artigo

if (melhor_k_estatistico != melhor_k_artigo) {
  message(
    sprintf(
      ">>> Nota: o k estatisticamente ótimo (silhouette) nesta execução é %d, diferente do k=3 do artigo. ",
      melhor_k_estatistico
    ),
    "Mantendo k=3 para fidelidade metodológica (Marques & Saraiva, 2017) — ",
    "ver 'grafico_silhouette' para decidir se vale usar o k estatístico em vez disso."
  )
}

set.seed(3)

ajuste_kmeans_k3 <- kmeans(
  variaveis_cluster_escaladas,
  centers = melhor_k_artigo,
  nstart = 25
)

hexagonos_positivos_com_cluster <-
  hexagonos_positivos |>
  drop_na(
    households_connected_to_water_supply_network,
    households_connected_to_sewerage_system,
    number_of_buildings,
    avg_building_area,
    number_private_residences,
    avg_slope_topodata
  ) |>
  mutate(cluster = factor(ajuste_kmeans_k3$cluster))

tabela_3_reconstruida <-
  hexagonos_positivos_com_cluster |>
  count(cluster, name = "n_amostras")

gt_tabela_3 <-
  tabela_3_reconstruida |>
  gt() |>
  tab_header(title = "Tabela 3 reconstruída — amostras por cluster")

print(gt_tabela_3)


estatisticas_cluster <-
  hexagonos_positivos_com_cluster |>
  group_by(cluster) |>
  summarise(across(
    c(
      households_connected_to_water_supply_network,
      households_connected_to_sewerage_system,
      number_of_buildings,
      avg_building_area,
      number_private_residences,
      avg_slope_topodata
    ),
    median,
    .names = "mediana_{.col}"
  ))
estatisticas_cluster

gt_estatisticas_cluster <-
  hexagonos_positivos_com_cluster |>
  group_by(cluster) |>
  summarise(across(
    c(
      households_connected_to_water_supply_network,
      households_connected_to_sewerage_system,
      number_of_buildings,
      avg_building_area,
      number_private_residences,
      avg_slope_topodata
    ),
    median,
    .names = "mediana_{.col}"
  )) |>
  gt() |>
  tab_header(
    title = "Estatísticas por cluster (equivalente descritivo à Fig. 11)"
  ) |>
  fmt_number(columns = -cluster, decimals = 2)

print(gt_estatisticas_cluster)


# ==============================================================================
# 14. EXPORTAÇÃO DO ARTEFATO DO MODELO FINAL (LINHA REPRODUÇÃO)
# ==============================================================================
artefato_modelo_final_reproducao <- list(
  limiar_herdado_pct = limiar_pct,
  limiar_decisao_padrao = LIMIAR_PADRAO,
  # limiar_fracao (fiel ao artigo, ~70%) — NÃO é mais o J de Youden
  limiar_decisao_artigo = LIMIAR_ARTIGO,
  # referência/diagnóstico: o 70% original do artigo
  limiar_decisao_otimizado = LIMIAR_OTIMIZADO,
  # idêntico a limiar_decisao_padrao nesta versão (mantido por retrocompatibilidade)
  avaliacao_limiares = avaliacao_limiares,
  tabela_b5_reconstruida = tabela_b5_reconstruida,
  vencedor_comparacao = vencedor_id,
  melhores_hiperparametros = melhores_hiperparametros,
  fit_final = fit_final,

  grafico_perfil_limiares_all = grafico_perfil_limiares_all,
  grafico_perfil_limiares = grafico_perfil_limiares,

  matriz_confusao_treino_70 = matriz_confusao_treino_70,
  matriz_confusao_treino_20 = matriz_confusao_treino_20,
  matriz_confusao_teste_70 = matriz_confusao_teste_70,
  matriz_confusao_teste_20 = matriz_confusao_teste_20,
  matriz_confusao_completa_70 = matriz_confusao_completa_70,
  matriz_confusao_completa_20 = matriz_confusao_completa_20,
  grafico_matrizes_confusao_dos_modelos = figura_final,

  tabela_generalizacao = tabela_generalizacao,
  tabela_comparativa_teste = tabela_comparativa_teste,
  tabela_comparativa_completa = tabela_comparativa_completa,
  tabela_falsos_positivos = tabela_falsos_positivos,

  importancia_shap = importancia_shap,
  grafico_shap_beeswarm = grafico_shap_beeswarm,
  grafico_shap_barras = grafico_shap_barras,

  erro_medio_aditividade_shap = erro_medio_aditividade,
  silhouette_por_k = silhouette_por_k,
  grafico_silhouette = grafico_silhouette,

  tabela_3_reconstruida = tabela_3_reconstruida,
  hexagonos_positivos_com_cluster = hexagonos_positivos_com_cluster,
  gt_estatisticas_cluster = gt_estatisticas_cluster
)

pasta_saida

dir.create(
  file.path(pasta_saida),
  recursive = TRUE,
  showWarnings = FALSE
)

write_rds(
  artefato_modelo_final_reproducao,
  # PARAMETRIZADO: nome agora inclui tipo_grid, para não sobrescrever o
  # resultado de uma variante (area_pura/ponderado_edificacoes) com a outra.
  file.path(
    pasta_saida,
    paste0("modelo_artigo_", tipo_grid, sufixo_saida, ".rds")
  )
)

# Superfície de probabilidade como GeoPackage, para visualização em QGIS
st_write(
  superficie_probabilidade,
  file.path(
    pasta_saida,
    paste0("superficie_probabilidade_artigo_", tipo_grid, sufixo_saida, ".gpkg")
  ),
  delete_dsn = TRUE
)


# Tabelas ---------------------------------------------------------------------------------
dir.create(file.path(pasta_saida, "modelo"), showWarnings = FALSE)
pasta_saida <- here("outputs", "artefato", "modelo")

# Exportação para Submissão/Leitura Humana (Arquivos Avulsos)
dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)
dir.create(file.path(pasta_saida, "figures"), showWarnings = FALSE)


readr::write_csv(
  tabela_b5_reconstruida,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_b5_reconstruida_modelo", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  tabela_generalizacao,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_generalizacao", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  tabela_comparativa_teste,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_modelo_teste", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  tabela_comparativa_completa,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_modelo_completa", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  tabela_falsos_positivos,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_falsos_positivos", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  importancia_shap,
  file.path(
    pasta_saida,
    "tables",
    paste0("importancia_shap", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  hexagonos_positivos_com_cluster,
  file.path(
    pasta_saida,
    "tables",
    paste0("hexagonos_positivos_com_cluster", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  importancia_shap,
  file.path(
    pasta_saida,
    "tables",
    paste0("importancia_shap", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  estatisticas_cluster,
  file.path(
    pasta_saida,
    "tables",
    paste0("estatisticas_cluster", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  tabela_3_reconstruida,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_kmeans", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  bins_padrao,
  file.path(
    pasta_saida,
    "tables",
    paste0("bins_padrao", sufixo_saida, ".csv")
  )
)


readr::write_csv(
  bins_20,
  file.path(
    pasta_saida,
    "tables",
    paste0("bins_20", sufixo_saida, ".csv")
  )
)


# Graficos ---------------------------------------------------------------------------------

# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_perfil_limiares_todas_as_metricas", sufixo_saida, ".png")
  ),
  plot = grafico_perfil_limiares_all,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300
)

# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_perfil_limiares_recall_e_precision", sufixo_saida, ".png")
  ),
  plot = grafico_perfil_limiares,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300
)


# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("figure_matriz_confusion", sufixo_saida, ".png")
  ),
  plot = figure_matriz_confusion,
  width = 28,
  height = 25,
  units = "cm",
  dpi = 300
)


# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("figure_matriz_confusion_article", sufixo_saida, ".png")
  ),
  plot = figure_matriz_confusion_article,
  width = 28,
  height = 25,
  units = "cm",
  dpi = 300
)

# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_shap_barras", sufixo_saida, ".png")
  ),
  plot = grafico_shap_barras,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300
)


# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_shap_beeswarm", sufixo_saida, ".png")
  ),
  plot = grafico_shap_beeswarm,
  width = 28,
  height = 25,
  units = "cm",
  dpi = 300
)


# Exportar em alta resolução no tamanho exato da coluna do SBSR
ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_silhouette", sufixo_saida, ".png")
  ),
  plot = grafico_silhouette,
  width = 28,
  height = 25,
  units = "cm",
  dpi = 300
)
