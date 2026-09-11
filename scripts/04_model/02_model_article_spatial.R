# ==============================================================================
# MODELO FINAL — v4 (DESVIO DELIBERADO DA LINHA REPRODUÇÃO, PRIORIZANDO
# DESEMPENHO PREDITIVO EM VEZ DE FIDELIDADE AO ARTIGO)
#
# A partir daqui este script deixa de ser a "linha Reprodução" fiel ao artigo
# e passa a incorporar práticas melhores de validação, por decisão explícita
# do usuário (aceitando perder fidelidade estrita ao texto original):
#
#   [C1] SPLIT ESPACIAL EM VEZ DE ALEATÓRIO: o split 80/20 aleatório (Seção 3
#        original) e o vfold_cv aleatório (Seções 5 e 7) ignoram autocorrelação
#        espacial — hexágonos vizinhos tendem a ser muito parecidos entre si
#        (mesma vizinhança, mesma infraestrutura, mesmo padrão urbano). Com
#        split aleatório, é comum um hexágono do treino ficar a poucos metros
#        de um hexágono do teste, praticamente "vazando" a resposta por
#        proximidade geográfica (o modelo não está generalizando para uma
#        área nova, está memorizando padrões locais que reaparecem a poucos
#        metros de distância). Trocado por blockCV::cv_spatial(), que separa
#        blocos geográficos inteiros entre treino/teste/folds, com blocos do
#        tamanho do alcance de autocorrelação espacial estimado via
#        variograma (automap) — a mesma técnica já usada na linha
#        modelo_artigo_buffer_simplificado_artigo.R.
#   [C2] Isso afeta: o split holdout (Seção 3), os folds da comparação de
#        algoritmos (Seção 5) e os folds do Grid Search (Seção 7). Os splits
#        continuam estratificados por is_slum (via 'column' do cv_spatial).
#
# HISTÓRICO (linha Reprodução original, v3, para rastreabilidade):
# Segue os passos descritos nas Seções 3.5-3.8 e Apêndice B do artigo:
#   - Comparação de 6 algoritmos via CV (Tabela B.5)
#   - Random Forest selecionado (maior recall, melhor equilíbrio com accuracy)
#   - Grid Search de hiperparâmetros
#   - [SUBSTITUÍDO em v4] Split 80/20 aleatório -> agora split espacial (C1)
#   - SMOTENC 80% (aumentado de 50% na v4 para melhorar recall), COM
#     normalização (adicionada numa revisão anterior; comentário do v3
#     estava desatualizado dizendo que não normalizava)
#   - Matriz de confusão no teste (Fig. 4a) e na base completa (Fig. 4b)
#   - SHAP via fastshap + shapviz (aproximação de Monte Carlo do
#     shap.TreeExplainer usado no artigo em Python — mesma base teórica,
#     implementação diferente)
#   - Clusterização k-means (k=3) sobre hexágonos classificados como favela
#
# DEPENDÊNCIA: este script espera que
# 02_article_sensibility_analysis_dt_multires.R já tenha rodado (com a
# MESMA 'resolucao_hex' definida na Seção 2 abaixo) e salvo
# 'sensibilidade_artigo<_res9|_res10|_res11>.rds' em here("outputs",
# "artefato") — o limiar de cobertura usado para rotular 'is_slum' é
# HERDADO desse artefato (via 'limiar_fracao'/'LIMIAR_ARTIGO'), não
# hardcoded aqui. Segue o mesmo padrão de caminhos
# (pasta_entrada/pasta_saida, mapa explícito de resolução sufixado) já
# adotado em 01_grid_dasimetrico_multires.R, 02_article_sensibility_
# analysis_dt_multires.R e modelo_artigo.R (linha Reprodução).
#
# [CORREÇÃO DE DISCLAIMER] O bloco abaixo, copiado da linha Reprodução,
# dizia que "CV espacial, normalização pré-SMOTE" NÃO estavam neste
# arquivo — mas CV espacial (C1/C2) e normalização são exatamente o que
# este script implementa. Apenas calibração de probabilidade, Índice de
# Moran nos resíduos e PR-AUC/Brier como critério primário continuam
# ausentes (trabalho futuro, fora do escopo deste script):
# O QUE NÃO ESTÁ AQUI DE PROPÓSITO: calibração de probabilidade, Índice de
# Moran nos resíduos, PR-AUC/Brier como critério primário.
#
# MUDANÇAS NESTA VERSÃO (v3), EM RELAÇÃO AO v2 RECEBIDO:
#   [B1] matriz_confusao_* nunca eram criadas (conf_mat() ausente) — o script
#        quebrava na exportação final, depois de já ter pago o custo do Grid
#        Search e do SHAP. Corrigido: todas as 6 matrizes (treino/teste/
#        completo x padrão/otimizado) são construídas explicitamente.
#   [B2] LIMIAR_DECISAO era referenciado na exportação mas a linha que o
#        definia estava comentada. Corrigido.
#   [B3] 'superficie_probabilidade_otimizada' era usada sem nunca ter sido
#        criada — só existia 'superficie_probabilidade'. Corrigido.
#   [B4] Ordem invertida na seção de falsos positivos: a mensagem usava
#        'falsos_positivos' antes de ele existir. Corrigido.
#   [B5] cut() dos bins "otimizados" tinha breaks fixos (c(LIMIAR_OTIMIZADO,
#        0.4, 0.5, ...)) que deixam de ser crescentes se LIMIAR_OTIMIZADO >=
#        0.4 — cut() quebraria. Corrigido para construir os cortes
#        dinamicamente a partir do limiar real encontrado.
#   [B6] VAZAMENTO NO THRESHOLD TUNING: o limiar de decisão ótimo era
#        escolhido olhando para 'preds_prob_teste' — o próprio conjunto de
#        teste depois usado para reportar desempenho "honesto". Corrigido:
#        o limiar agora é escolhido sobre as predições OUT-OF-FOLD do Grid
#        Search (nunca vistas como um bloco coeso pelo modelo final), e o
#        teste populações permanece intocado até a avaliação final.
#   [B7] Blocos de f_meas duplicados (treino/teste calculados duas vezes,
#        idênticos) — removida a duplicação.
#   [B8] recall_treino/accuracy_treino/etc. eram calculados e descartados —
#        agora entram numa tabela de 3 cenários (Treino/Teste/Completo), que
#        é exatamente o diagnóstico de overfitting que o artigo usa na Seção
#        4.2 ("recall dropping from 95.9%... to 86.5%...").
#   [B9] Tabelas trocadas de message()+kable() para gt (mais funcional,
#        formatação de %, destaque da melhor linha) — pedido explícito.
#   [B10] SHAP reforçado: nsim maior, amostra maior, checagem de aditividade
#        (soma dos SHAP + baseline ≈ probabilidade prevista) e gráfico de
#        beeswarm de verdade via shapviz (o v2 só tinha uma tabela de médias).
#   [B11] LIMIAR DE DECISÃO MAIS FIEL AO TEXTO DO ARTIGO. A versão anterior
#        usava LIMIAR_PADRAO = 0.5 (convenção genérica de classificação
#        binária) como o limiar "principal" de decisão sobre a probabilidade
#        prevista, e tratava o limiar de 70% só como referência textual. Mas
#        a Seção 3.6 do artigo diz literalmente: "The measurements were
#        performed with the 70% probability threshold from the model
#        results, based in the results of the sensitivity analysis in
#        Section 3.3." Ou seja: o artigo usa o MESMO número (70%) tanto como
#        limiar de cobertura do rótulo (Seção 3.3) quanto como limiar de
#        decisão sobre a probabilidade prevista (Seção 3.6) — uma escolha
#        pouco convencional (a maioria dos pipelines usaria 0.5 por padrão
#        e reservaria 70% só para o rótulo), mas é o que o texto afirma.
#        Para replicar o mais fielmente possível, LIMIAR_PADRAO agora É
#        limiar_fracao (herdado da sensibilidade, tipicamente 0.70) — não
#        mais um valor fixo de 0.5. O limiar 0.5 permanece disponível só
#        como referência opcional no gráfico de perfil (Seção 8), não como
#        resultado principal. Como consequência, a clusterização k-means
#        (Seção 13) também passou a usar a decisão baseada nesse limiar
#        fiel ao artigo (classe_decisao), em vez do limiar otimizado por J
#        de Youden (classe_decisao_otimizada) — o J de Youden continua
#        calculado e reportado, mas como diagnóstico complementar, já que
#        não é um critério mencionado no texto do artigo.
# ==============================================================================

# ==============================================================================
# 1. PACOTES
# ==============================================================================
gc()

if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  tidymodels,
  themis,
  sf,
  workflowsets,
  patchwork,
  future,
  rpart, # Decision Tree
  ranger, # Random Forest
  xgboost, # XGBoost
  C50, # boosting via C5.0 -> aproximação de AdaBoost
  bonsai, # engine lightgbm para boost_tree() -> aproximação de Gradient Boosting
  lightgbm,
  kknn, # KNN
  fastshap, # SHAP (Monte Carlo) para o modelo final
  shapviz, # [B10] visualização de SHAP (beeswarm/bar) compatível com fastshap
  cluster, # silhouette() para validar k do k-means
  gt, # [B9] tabelas funcionais, sem depender de message()+kable()
  blockCV, # [C1] CV espacial (split e folds em blocos geográficos)
  gstat, # [C1] variograma para estimar alcance de autocorrelação espacial
  automap # [C1] ajuste automático do variograma (autofitVariogram)
)

# ==============================================================================
# 2. IMPORTAÇÃO — BASE + ARTEFATO DE SENSIBILIDADE (HERDA O LIMIAR)
# ==============================================================================
pasta_entrada <- here("data", "processed")
pasta_saida <- here("outputs", "artefato")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# Mesmo padrão de resolução do 01_grid_dasimetrico_multires.R e do
# 02_article_sensibility_analysis_dt_multires.R: mapa explícito (sem
# função com regra especial escondida), sempre sufixado, rodando este
# script individualmente para cada resolução (9, 10 ou 11).
resolucao_hex <- 10 # <- mude aqui (9, 10 ou 11) e rode de novo a cada vez

arquivos_entrada <- c(
  "9" = "grid_hexagonal_res9.rds",
  "10" = "grid_hexagonal_artigo.rds",
  "11" = "grid_hexagonal_res11.rds"
)
arquivo_entrada <- arquivos_entrada[[as.character(resolucao_hex)]]
sufixo_saida <- paste0("_res", resolucao_hex)

caminho_sensibilidade <- file.path(
  pasta_saida,
  paste0("sensibilidade_artigo", sufixo_saida, ".rds")
)

if (!file.exists(caminho_sensibilidade)) {
  stop(
    "Artefato de sensibilidade não encontrado em ",
    caminho_sensibilidade,
    ". ",
    "Rode 02_article_sensibility_analysis_dt_multires.R primeiro (com a ",
    "mesma 'resolucao_hex' definida aqui) — este script depende do limiar ",
    "de cobertura decidido lá."
  )
}

artefato_sensibilidade <- readRDS(caminho_sensibilidade)

# O limiar herdado é o vencedor real do artefato (ver 'comparacao_limiares'
# se existir; senão cai para 'limiar_escolhido', que na v5 é o 70% fixo do
# artigo mantido como referência textual).
limiar_id <- if (!is.null(artefato_sensibilidade$melhor_por_trade_off)) {
  artefato_sensibilidade$melhor_por_trade_off$Threshold[1]
} else {
  artefato_sensibilidade$limiar_escolhido
}

limiar_pct <- as.numeric(str_extract(as.character(limiar_id), "\\d+"))
limiar_fracao <- limiar_pct / 100

message(sprintf(
  ">>> Limiar de cobertura espacial herdado da sensibilidade: %d%% (id: %s)",
  limiar_pct,
  limiar_id
))

grid_hexagonal <- readRDS(file.path(pasta_entrada, arquivo_entrada))

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
# 3. SPLIT ESPACIAL (C1) — substitui o split 80/20 aleatório da linha Reprodução
# ==============================================================================
# TEORIA — por que split aleatório é problemático aqui: hexágonos vizinhos
# tendem a compartilhar infraestrutura, morfologia urbana e características
# socioeconômicas (autocorrelação espacial positiva — Tobler, 1970: "tudo
# está relacionado a tudo, mas coisas próximas estão mais relacionadas que
# coisas distantes"). Com split aleatório, um hexágono de treino pode ficar
# a poucos metros de um hexágono de teste, e o modelo "generaliza" bem no
# teste não porque aprendeu o padrão geral de favelas, mas porque já viu
# (quase) a mesma vizinhança no treino — uma forma de vazamento por
# proximidade espacial. A correção é blockCV::cv_spatial(): separa BLOCOS
# geográficos inteiros entre treino e teste, com blocos do tamanho do
# alcance de autocorrelação espacial (estimado abaixo via variograma).
BUFFER_PADRAO_M <- 300
N_AMOSTRA_VARIOGRAMA <- 2000

estimar_alcance_variograma <- function(dados_sf, coluna, n_amostra) {
  dados_pontos <- dados_sf |>
    st_centroid() |>
    select(all_of(coluna))

  if (nrow(dados_pontos) > n_amostra) {
    set.seed(42)
    dados_pontos <- dados_pontos[sample.int(nrow(dados_pontos), n_amostra), ]
  }

  formula_variograma <- as.formula(paste(coluna, "~ 1"))
  ajuste <- automap::autofitVariogram(
    formula_variograma,
    input_data = as(dados_pontos, "Spatial")
  )
  alcance <- ajuste$var_model$range[2] # linha 2 = componente estruturado (não o nugget)

  if (!is.finite(alcance) || alcance <= 0) {
    stop("Variograma ajustado não produziu um alcance válido.")
  }
  alcance
}

autocor <- tryCatch(
  estimar_alcance_variograma(
    dados_base,
    "number_of_buildings",
    N_AMOSTRA_VARIOGRAMA
  ),
  error = function(e) {
    message("Estimativa de autocorrelação falhou: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(autocor)) {
  buffer_espacial <- autocor
  message(sprintf(
    "Alcance de autocorrelação (tamanho do bloco espacial): ~%.0f m",
    buffer_espacial
  ))
} else {
  buffer_espacial <- BUFFER_PADRAO_M
  message(sprintf(
    "Autocorrelação não estimada — usando bloco padrão de %d m.",
    BUFFER_PADRAO_M
  ))
}

# Converte os folds do blockCV (listas de índices) em objetos rset do
# rsample, compatíveis com analysis()/assessment()/tune_grid()/workflow_map().
# 'dados_referencia' é a versão SEM geometria (recipes/modelos não lidam bem
# com a coluna sfc); os índices vêm da versão COM geometria, mas como as
# linhas estão na mesma ordem nas duas versões, os índices são válidos nas
# duas — assim resolvemos o split espacial sem carregar geometria para
# dentro do recipe/tune_grid.
converter_blockcv_para_rset <- function(dados_referencia, blockcv_obj) {
  splits <- purrr::map(blockcv_obj$folds_list, function(fold) {
    rsample::make_splits(
      list(analysis = fold[[1]], assessment = fold[[2]]),
      data = dados_referencia
    )
  })
  rsample::manual_rset(splits, ids = paste0("Fold", seq_along(splits)))
}

dados_base_sem_geom <- dados_base |> st_drop_geometry()

# k=5 aproxima a proporção 80/20 do split original (4 blocos = treino, 1 = teste)
set.seed(42)
blockcv_holdout <- blockCV::cv_spatial(
  x = dados_base,
  column = "is_slum",
  size = buffer_espacial,
  k = 5,
  hexagon = FALSE,
  selection = "random",
  iteration = 50,
  progress = FALSE,
  report = FALSE
)

split_inicial_espacial <- converter_blockcv_para_rset(
  dados_base_sem_geom,
  blockcv_holdout
)

dados_treino <- analysis(split_inicial_espacial$splits[[1]])
dados_teste <- assessment(split_inicial_espacial$splits[[1]])

# Versões COM geometria dos mesmos subconjuntos (necessárias para calcular os
# blocos espaciais do Grid Search na Seção 7, sobre dados_treino apenas)
dados_treino_sf <- dados_base[split_inicial_espacial$splits[[1]]$in_id, ]

pct_slum_treino <- mean(dados_treino$is_slum == "Slum")
pct_slum_teste <- mean(dados_teste$is_slum == "Slum")
message(sprintf(
  "Treino: %d hexágonos (%.1f%% favela) | Teste: %d hexágonos (%.1f%% favela)",
  nrow(dados_treino),
  pct_slum_treino * 100,
  nrow(dados_teste),
  pct_slum_teste * 100
))

# ==============================================================================
# 4. RECEITA DE PRÉ-PROCESSAMENTO
# ==============================================================================
# [CORREÇÃO DE COMENTÁRIO] O texto abaixo dizia "sem step_normalize()", mas a
# receita logo adiante TEM step_normalize(). Nesta v4 (que já abandonou
# fidelidade estrita ao artigo) a normalização fica mesmo, por ser
# tecnicamente necessária (KNN e distâncias do SMOTENC são sensíveis a
# escala; sem normalizar, uma variável em milhares como number_of_buildings
# dominaria sozinha qualquer cálculo de distância).
# TEORIA — por que SMOTENC (e não SMOTE simples): as variáveis de entrada
# misturam contínuas (ex.: avg_slope_topodata) e categóricas/lógicas (ex.:
# highway_intersection). SMOTE puro assume espaço totalmente numérico; SMOTENC
# (Chawla et al., 2002, extensão para dados mistos) preserva a natureza
# categórica ao gerar sintéticos, evitando "meias-categorias" sem sentido.
receita <- recipe(is_slum ~ ., data = dados_treino) |>
  update_role(id_hex, new_role = "id") |>
  step_rm(slum_coverage_pct) |>
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.90, method = "spearman") |>
  step_normalize(all_numeric_predictors()) |>
  step_smotenc(is_slum, over_ratio = 0.8) |> # [MELHORIA] aumentado de 0.5 para 0.8: mais exemplos sintéticos da classe rara
  step_dummy(all_nominal_predictors()) # converte fatores em 0/1

# ==============================================================================
# 5. COMPARAÇÃO DE ALGORITMOS (reproduz a Tabela B.5 / Apêndice B)
# ==============================================================================
# O artigo compara 6 colunas na Tabela B.5 (Decision tree, Random forest,
# Gradient boosting, AdaBoost, XGBoost, KNN), embora o texto do Apêndice B
# diga "seven different models" — inconsistência do próprio artigo entre
# texto e tabela, mantida aqui como está (6 modelos, os que aparecem na tabela).
# "Gradient boosting" e "AdaBoost" não têm engine nativa 1:1 no tidymodels;
# aproximamos por LightGBM (gradient boosting genérico) e C5.0 com boosting
# (mecanismo de reponderação por erro, no espírito do AdaBoost) — mesma
# aproximação já documentada e usada em modelo_final_v7.R.
especificacoes_modelos <- list(
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

wf_set_comparacao <- workflow_set(
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
blockcv_comparacao <- blockCV::cv_spatial(
  x = dados_base,
  column = "is_slum",
  size = buffer_espacial,
  k = 10,
  hexagon = FALSE,
  selection = "random",
  iteration = 50,
  progress = FALSE,
  report = FALSE
)

folds_comparacao <-
  converter_blockcv_para_rset(
    dados_base_sem_geom,
    blockcv_comparacao
  )

future::plan(multisession, workers = max(1, availableCores() - 1))

resultados_comparacao <- workflow_map(
  wf_set_comparacao,
  fn = "fit_resamples",
  resamples = folds_comparacao,
  metrics = metric_set(accuracy, precision, recall, roc_auc, f_meas),
  control = control_resamples(save_pred = TRUE)
)
resultados_comparacao
future::plan(sequential)

tabela_b5_reconstruida <-
  collect_metrics(resultados_comparacao) |>
  select(wflow_id, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  arrange(desc(recall)) |>
  select(wflow_id, accuracy, precision, recall, roc_auc, f_meas)

gt_tabela_b5 <-
  tabela_b5_reconstruida |>
  gt() |>
  tab_header(
    title = "Tabela B.5 reconstruída",
    subtitle = "Comparação de algoritmos via 10-fold CV (base completa)"
  ) |>
  fmt_percent(
    columns = c(accuracy, precision, recall, roc_auc, f_meas),
    decimals = 1
  ) |>
  data_color(
    columns = recall,
    colors = scales::col_numeric(palette = c("white", "#2c7fb8"), domain = NULL)
  ) |>
  tab_footnote(
    footnote = paste0(
      "Métricas calculadas com limiar de probabilidade = ",
      round(0.5 * 100),
      "% (Seção 3.6 do artigo)."
    ),
    locations = cells_column_labels(columns = c(recall, precision))
  )
print(gt_tabela_b5)

# ==============================================================================
# 6. SELEÇÃO DO MODELO (artigo: "Random Forest... optimal balance between
#    accuracy and recall")
# ==============================================================================
vencedor_id <-
  tabela_b5_reconstruida |>
  slice_max(recall, n = 1) |>
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
# 7. GRID SEARCH (Seção 3.5: "a Grid Search was conducted to optimize its
#    parameters")
# ==============================================================================
n_preditores_grid <- sum(
  summary(prep(receita))$role == "predictor",
  na.rm = TRUE
)

# [MELHORIA DE PREVISÃO] grid_regular(levels=3) testa só 27 combinações
# (3^3), numa grade grosseira e pouco eficiente em espaço de 3 dimensões.
# Trocado por grid_space_filling, que cobre o espaço de hiperparâmetros de
# forma muito mais eficiente com mais pontos testados — maior chance de
# achar uma combinação de fato boa. Range de mtry ampliado até quase o
# total de preditores (em vez de fixo em 12).
grade_hiperparametros <-
  grid_space_filling(
    mtry(range = c(2, max(2, n_preditores_grid - 1))),
    trees(range = c(300, 1200)),
    min_n(range = c(2, 20)),
    size = 30
  )

set.seed(456)
blockcv_grid_search <- blockCV::cv_spatial(
  x = dados_treino_sf,
  column = "is_slum",
  size = buffer_espacial,
  k = 10,
  hexagon = FALSE,
  selection = "random",
  iteration = 50,
  progress = FALSE,
  report = FALSE
)

folds_grid_search <- converter_blockcv_para_rset(
  dados_treino,
  blockcv_grid_search
)

future::plan(multisession, workers = max(1, availableCores() - 1))

# [B6] save_pred = TRUE é essencial aqui: é dessas predições out-of-fold (cada
# hexágono de treino previsto por uma árvore que NÃO o viu) que vamos extrair
# o limiar de decisão ótimo na Seção 8 — sem contaminar o holdout de teste.
resultado_grid_search <- tune_grid(
  wf_final_base,
  resamples = folds_grid_search,
  grid = grade_hiperparametros,
  metrics = metric_set(accuracy, precision, recall, roc_auc, f_meas),
  control = control_grid(save_pred = TRUE)
)

future::plan(sequential)

melhores_hiperparametros <- select_best(
  resultado_grid_search,
  metric = "recall"
)

message(">>> Melhores hiperparâmetros (Grid Search, critério: recall):")
print(melhores_hiperparametros)

wf_final <- finalize_workflow(wf_final_base, melhores_hiperparametros)

# ==============================================================================
# 8. AJUSTE FINAL + LIMIAR DE DECISÃO (SEM VAZAMENTO) [B6][B11]
# ==============================================================================
# [MELHORIA] LIMIAR_PADRAO é redefinido logo abaixo, depois de calculado o
# limiar estatisticamente ótimo (J de Youden) — ver nota completa após o
# cálculo de LIMIAR_OTIMIZADO.

fit_final <- fit(wf_final, data = dados_treino)

# --- TEORIA: por que otimizar o limiar sobre OOF, não sobre o teste [B6] ---
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
# NOTA DE FIDELIDADE [B11]: o limiar abaixo (J de Youden, sobre OOF) é uma
# ANÁLISE COMPLEMENTAR, não mencionada no artigo — o texto não nomeia
# nenhuma estatística para escolher o ponto de corte, só afirma que 70% foi
# usado. Ele é mantido como diagnóstico de robustez ("será que 70% é mesmo
# uma boa escolha de limiar de decisão, independentemente de já ser o
# limiar de cobertura do rótulo?"), mas não substitui LIMIAR_PADRAO como
# resultado principal deste script.
preds_oof_treino <- collect_predictions(
  resultado_grid_search,
  parameters = melhores_hiperparametros
)

avaliacao_limiares <- probably::threshold_perf(
  preds_oof_treino,
  truth = is_slum,
  estimate = .pred_Slum,
  thresholds = seq(0.05, 0.95, by = 0.01),
  metrics = metric_set(j_index, f_meas, accuracy, sens, spec, precision),
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

# [MELHORIA DE PREVISÃO] Como este script agora prioriza desempenho
# preditivo (não mais fidelidade estrita ao texto do artigo), o limiar
# PRINCIPAL passa a ser o estatisticamente ótimo (J de Youden, calculado
# sobre predições OOF do Grid Search — sem vazamento), em vez do 70%
# reaproveitado da Seção 3.3 do artigo. O 70% do artigo continua disponível
# como referência em LIMIAR_ARTIGO, para quem quiser comparar.
LIMIAR_ARTIGO <- limiar_fracao
LIMIAR_PADRAO <- LIMIAR_OTIMIZADO

message(sprintf(
  ">>> Limiar principal (estatisticamente ótimo, J de Youden, predições OOF do Grid Search): %.2f (J = %.4f)",
  LIMIAR_PADRAO,
  melhor_limiar_obj$.estimate[1]
))

message(sprintf(
  ">>> Limiar do artigo (Seção 3.6, referência/diagnóstico — não é mais o principal nesta versão): %.2f",
  LIMIAR_ARTIGO
))

if (abs(LIMIAR_OTIMIZADO - LIMIAR_ARTIGO) < 0.05) {
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
grafico_perfil_limiares <-
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
      "recall" = "Sensibilidade (Recall)",
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
    label = "J de Youden (diagnóstico)",
    hjust = -0.1,
    color = "firebrick",
    size = 5
  ) +
  annotate(
    "text",
    x = LIMIAR_PADRAO,
    y = 0.05,
    label = "limiar do artigo",
    hjust = 1.1,
    color = "gray20",
    size = 5
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0,
    label = "convenção 0.5 (referência)",
    hjust = -0.1,
    color = "gray60",
    size = 5
  ) +
  labs(
    title = "Perfil de limiares de decisão (predições OOF do Grid Search)",
    subtitle = "Linha sólida = limiar principal (fiel ao artigo, Seção 3.6) | Linha tracejada = J de Youden (diagnóstico)",
    x = "Limiar de probabilidade",
    y = "Valor da métrica",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16)
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
      classe_decisao = factor(
        if_else(.pred_Slum >= LIMIAR_PADRAO, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      ),
      # [MELHORIA] Agora representa o limiar do ARTIGO (70%), mantido como
      # referência/diagnóstico — o limiar PRINCIPAL (classe_decisao) passou
      # a ser o estatisticamente ótimo (J de Youden), ver Seção 8.
      classe_decisao_otimizada = factor(
        if_else(.pred_Slum >= LIMIAR_ARTIGO, "Slum", "Non_Slum"),
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

# ==============================================================================
# 9. MATRIZES DE CONFUSÃO (Fig. 4a e 4b) [B1] — nunca eram criadas no v2
# ==============================================================================
matriz_confusao_treino_padrao <- conf_mat(
  preds_treino,
  truth = is_slum,
  estimate = classe_decisao
)
matriz_confusao_treino_otimizada <- conf_mat(
  preds_treino,
  truth = is_slum,
  estimate = classe_decisao_otimizada
)

matriz_confusao_teste_padrao <- conf_mat(
  preds_teste,
  truth = is_slum,
  estimate = classe_decisao
)
matriz_confusao_teste_otimizada <- conf_mat(
  preds_teste,
  truth = is_slum,
  estimate = classe_decisao_otimizada
)

matriz_confusao_completa_padrao <- conf_mat(
  preds_completo,
  truth = is_slum,
  estimate = classe_decisao
)
matriz_confusao_completa_otimizada <- conf_mat(
  preds_completo,
  truth = is_slum,
  estimate = classe_decisao_otimizada
)

# Mantém os nomes usados pelo v2 no artefato final (retrocompatibilidade)
matriz_confusao_teste <- matriz_confusao_teste_padrao
matriz_confusao_completa <- matriz_confusao_completa_padrao

matriz_confusao_teste
matriz_confusao_teste_otimizada
matriz_confusao_completa
matriz_confusao_completa_otimizada

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
matriz_confusao_teste <- preparar_dados_cm(matriz_confusao_teste)
matriz_confusao_teste_otimizada <- preparar_dados_cm(
  matriz_confusao_teste_otimizada
)
matriz_confusao_completa <- preparar_dados_cm(matriz_confusao_completa)
matriz_confusao_completa_otimizada <- preparar_dados_cm(
  matriz_confusao_completa_otimizada
)

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
      guide = guide_colorbar(
        barheight = grid::unit(10, "cm")
      )
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
p1 <- plotar_cm(matriz_confusao_teste, "Confusion Matrix with test data")
p2 <- plotar_cm(
  matriz_confusao_teste_otimizada,
  "Confusion Matrix with test Optimized data"
)
p3 <- plotar_cm(matriz_confusao_completa, "Confusion Matrix with all data")
p4 <- plotar_cm(
  matriz_confusao_completa_otimizada,
  "Confusion Matrix with all Optimized data"
)

# Combinar com patchwork (lado a lado)
figura_final <- p1 + p2 + p3 + p4

# Exibir
print(figura_final)


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

# [B7][B8] Antes: cada métrica calculada em blocos repetidos (às vezes duas
# vezes) e as métricas de TREINO eram descartadas. Agora: uma função só,
# reaproveitada para os 3 cenários x 2 limiares, e o treino entra na tabela —
# é o diagnóstico de overfitting que o próprio artigo usa (Seção 4.2).

# treino
metricas_treino_padrao <- calcular_metricas(
  preds_treino,
  classe_decisao,
  "Treino (limiar do artigo)"
)
metricas_treino_padrao

metricas_treino_otimizado <- calcular_metricas(
  preds_treino,
  classe_decisao_otimizada,
  "Treino (limiar otimizado, diagnóstico)"
)
metricas_treino_otimizado

# teste
metricas_teste_padrao <- calcular_metricas(
  preds_teste,
  classe_decisao,
  "Teste (limiar do artigo)"
)
metricas_teste_padrao

metricas_teste_otimizado <- calcular_metricas(
  preds_teste,
  classe_decisao_otimizada,
  "Teste (limiar otimizado, diagnóstico)"
)
metricas_teste_otimizado

metricas_completo_padrao <- calcular_metricas(
  preds_completo,
  classe_decisao,
  "Completo (limiar do artigo)"
)
metricas_completo_padrao

metricas_completo_otimizado <- calcular_metricas(
  preds_completo,
  classe_decisao_otimizada,
  "Completo (limiar otimizado, diagnóstico)"
)
metricas_completo_otimizado

# Valores extraídos do artigo original — TESTE (Fig. 4a): TP=421, FN=66, TN=9359, FP=1125
recall_artigo_teste <- 0.8645
accuracy_artigo_teste <- (9359 + 421) / (9359 + 1125 + 66 + 421)
precision_artigo_teste <- 421 / (421 + 1125)
f_meas_artigo_teste <- 2 *
  (precision_artigo_teste * recall_artigo_teste) /
  (precision_artigo_teste + recall_artigo_teste)

# Valores extraídos do artigo original — BASE COMPLETA (Fig. 4b): TP=2059, FN=345, TN=46881, FP=5570
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

# --- [B8] Tabela 1: diagnóstico Treino -> Teste -> Completo (generalização) ---
tabela_generalizacao <- bind_rows(
  metricas_treino_padrao,
  metricas_teste_padrao,
  metricas_completo_padrao
)

# --- Tabela 2: teste, padrão vs. otimizado vs. artigo ---
tabela_comparativa_teste <- bind_rows(
  metricas_teste_padrao,
  metricas_teste_otimizado,
  linha_artigo_teste
)

# --- Tabela 3: base completa, padrão vs. otimizado vs. artigo ---
tabela_comparativa_completa <- bind_rows(
  metricas_completo_padrao,
  metricas_completo_otimizado,
  linha_artigo_completo
)

# [B9] gt no lugar de message()+kable(): tabelas reais, com % formatado e a
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
gt_teste <- construir_gt_comparativo(
  tabela_comparativa_teste,
  "Avaliação no conjunto de teste (holdout 20%)",
  "Esta é a avaliação mais honesta do modelo — simula dados nunca vistos"
)
gt_completo <- construir_gt_comparativo(
  tabela_comparativa_completa,
  "Avaliação na base completa (treino + teste)",
  "Contém viés de otimismo (80% dos dados já 'vistos' pelo modelo) — usada no artigo para a superfície probabilística"
)

print(gt_generalizacao)
print(gt_teste)
print(gt_completo)

# ==============================================================================
# 10. SUPERFÍCIE DE PROBABILIDADE — BINS (Tabela 2) E MAPA (Fig. 5)
# ==============================================================================
cortes_referencia <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

superficie_probabilidade <-
  dados_base |>
  select(id_hex, geometry, is_slum) |>
  inner_join(
    preds_completo |>
      select(id_hex, .pred_Slum, classe_decisao, classe_decisao_otimizada),
    by = "id_hex"
  ) |>
  mutate(
    # [B11] Estes bins são a Tabela 2 do artigo (Seção 4.3): cinco faixas
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
    # [B5] Bin para o limiar otimizado — construído DINAMICAMENTE a partir de
    # onde LIMIAR_OTIMIZADO realmente cai. A versão anterior fixava o segundo
    # corte em 0.4, o que quebra se LIMIAR_OTIMIZADO >= 0.4 (breaks deixam de
    # ser crescentes). cut() sem labels manuais gera rótulos de intervalo
    # automaticamente (ex.: "[0.28,0.4)"), sempre corretos independentemente
    # de onde o limiar cair.
    bin_probabilidade_otimizado = cut(
      .pred_Slum,
      breaks = unique(c(
        LIMIAR_OTIMIZADO,
        cortes_referencia[cortes_referencia > LIMIAR_OTIMIZADO]
      )),
      include.lowest = TRUE,
      right = FALSE
    )
  )

gt_bins_padrao <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_padrao)) |>
  count(bin_probabilidade_padrao, name = "n_hexagonos") |>
  gt() |>
  tab_header(
    title = "Distribuição por bin de probabilidade — limiar padrão (>= 50%)"
  )

gt_bins_otimizado <-
  superficie_probabilidade |>
  st_drop_geometry() |>
  filter(!is.na(bin_probabilidade_otimizado)) |>
  count(bin_probabilidade_otimizado, name = "n_hexagonos") |>
  gt() |>
  tab_header(
    title = sprintf(
      "Distribuição por bin de probabilidade — limiar otimizado (>= %.2f)",
      LIMIAR_OTIMIZADO
    )
  )

print(gt_bins_padrao)
print(gt_bins_otimizado)

# ==============================================================================
# 11. MAPA DE FALSOS POSITIVOS (Fig. 6)
# ==============================================================================
# [B4] Ordem corrigida: calcula 'falsos_positivos' (padrão) ANTES de usá-lo em
# qualquer mensagem — no v2 a ordem estava invertida e a mensagem do cenário
# otimizado referenciava um objeto ainda inexistente.
falsos_positivos <-
  superficie_probabilidade |>
  filter(classe_decisao == "Slum", is_slum == "Non_Slum")

falsos_positivos_otimizado <-
  superficie_probabilidade |>
  filter(classe_decisao_otimizada == "Slum", is_slum == "Non_Slum")

tabela_falsos_positivos <- tibble(
  Cenario = c(
    sprintf("Limiar principal, otimizado (%.2f)", LIMIAR_PADRAO),
    sprintf("Limiar do artigo, referência (%.0f%%)", LIMIAR_ARTIGO * 100)
  ),
  N_Falsos_Positivos = c(
    nrow(falsos_positivos),
    nrow(falsos_positivos_otimizado)
  ),
  Pct_Total_Hexagonos = c(
    nrow(falsos_positivos) / nrow(superficie_probabilidade),
    nrow(falsos_positivos_otimizado) / nrow(superficie_probabilidade)
  )
)

gt_falsos_positivos <- tabela_falsos_positivos |>
  gt() |>
  tab_header(
    title = "Falsos positivos identificados na base completa (Fig. 6)"
  ) |>
  fmt_percent(columns = Pct_Total_Hexagonos, decimals = 2) |>
  fmt_number(columns = N_Falsos_Positivos, decimals = 0)

print(gt_falsos_positivos)

# ==============================================================================
# 12. SHAP — IMPORTÂNCIA DE FEATURES (Fig. 9) [B10] — reforçado
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
#   - Visualização de verdade via shapviz: o v2 só produzia uma tabela de
#     |SHAP| médio — sem o gráfico beeswarm, a leitura de DIREÇÃO do efeito
#     (feature alta empurra para "Slum" ou para "Non_Slum"?) fica invisível,
#     que é exatamente o que a Fig. 9 do artigo mostra.
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
objeto_shapviz <- shapviz(valores_shap, X = X_shap, baseline = baseline_shap)

glimpse(objeto_shapviz$X)

grafico_shap_beeswarm <-
  sv_importance(
    objeto_shapviz,
    kind = "beeswarm",
    max_display = 10
  ) +
  labs(title = "SHAP beeswarm — equivalente à Fig. 9 do artigo") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
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
grafico_shap_beeswarm

grafico_shap_barras <-
  sv_importance(
    objeto_shapviz,
    kind = "bar",
    max_display = 10
  ) +
  labs(title = "Importância média |SHAP| por feature") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
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

print(grafico_shap_beeswarm)
print(grafico_shap_barras)

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
  filter(classe_decisao == "Slum") |>
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
variaveis_cluster_escaladas <- scale(variaveis_cluster)

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
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
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
  limiar_decisao_padrao = LIMIAR_PADRAO, # [MELHORIA] agora = limiar estatisticamente ótimo (J de Youden), não mais o 70% do artigo
  limiar_decisao_artigo = LIMIAR_ARTIGO, # referência/diagnóstico: o 70% original do artigo
  limiar_decisao_otimizado = LIMIAR_OTIMIZADO, # idêntico a limiar_decisao_padrao nesta versão (mantido por retrocompatibilidade)
  avaliacao_limiares = avaliacao_limiares,
  tabela_b5_reconstruida = tabela_b5_reconstruida,
  vencedor_comparacao = vencedor_id,
  melhores_hiperparametros = melhores_hiperparametros,
  fit_final = fit_final,

  grafico_perfil_limiares = grafico_perfil_limiares,

  matriz_confusao_treino_padrao = matriz_confusao_treino_padrao,
  matriz_confusao_treino_otimizada = matriz_confusao_treino_otimizada,
  matriz_confusao_teste_padrao = matriz_confusao_teste_padrao, # [B1]
  matriz_confusao_teste_otimizada = matriz_confusao_teste_otimizada, # [B1]
  matriz_confusao_completa_padrao = matriz_confusao_completa_padrao, # [B1]
  matriz_confusao_completa_otimizada = matriz_confusao_completa_otimizada, # [B1]
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

dir.create(
  file.path(pasta_saida),
  recursive = TRUE,
  showWarnings = FALSE
)

write_rds(
  artefato_modelo_final_reproducao,
  file.path(
    pasta_saida,
    paste0("modelo_artigo_melhorado_espacial", sufixo_saida, ".rds")
  )
)

# Superfície de probabilidade como GeoPackage, para visualização em QGIS
st_write(
  superficie_probabilidade,
  file.path(
    pasta_saida,
    paste0(
      "superficie_probabilidade_artigo_melhorado_espacial",
      sufixo_saida,
      ".gpkg"
    )
  ),
  delete_dsn = TRUE
)

message(">>> Modelo final (linha Melhoria, split/CV espacial) concluído e salvo com sucesso.")