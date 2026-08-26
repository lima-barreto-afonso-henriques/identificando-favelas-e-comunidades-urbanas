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
  parallelly
)
