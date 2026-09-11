# ==============================================================================
# Script: 00_setup.R
# Objetivo: Carregar todos os pacotes necessários para o projeto
# ==============================================================================
if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  tidymodels,
  geobr,
  censobr,
  sf,
  gt,
  h3jsr,
  skimr,
  vroom,
  assertthat,
  units,
  terra,
  viridis,
  osmdata,
  arrow,
  yaml,
  ggcorrplot,
  patchwork,
  spatialsample,
  future,
  themis,
  blockCV,
  parallelly,
  rpart, # Decision Tree
  ranger, # Random Forest
  xgboost, # XGBoost
  C50, # boosting via C5.0 -> aproximação de AdaBoost
  bonsai, # engine lightgbm para boost_tree() -> aproximação de Gradient Boosting
  lightgbm,
  kknn, # KNN
  workflowsets,
  cluster, # silhouette() para validar k do k-means
  fastshap, # SHAP (Monte Carlo) para o modelo final
  shapviz, # visualização de SHAP (beeswarm/bar) compatível com fastshap
  duckdb,
  DBI,
  furrr
)
