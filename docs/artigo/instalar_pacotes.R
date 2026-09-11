# =============================================================================
# instalar_pacotes.R
#
# Rode este script UMA VEZ, antes de abrir o projeto pela primeira vez, para
# instalar todas as dependências do artigo de uma só vez.
#
# Isso é diferente do chunk `pacotes` dentro do .qmd: aquele chunk também
# carrega os pacotes (com require/p_load) toda vez que o documento é
# renderizado, e precisa continuar vivendo dentro do .qmd — ele não pode
# depender de here::here() para se localizar, porque o pacote `here` é um
# dos pacotes que ele mesmo instala (ordem de bootstrap).
# =============================================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  knitr, tidyverse, broom, car, stargazer, report, skimr, here,
  sf, sp, spdep, spatialreg, lmtest, sandwich, ggeffects, kableExtra,
  corrr, scales, marginaleffects, gridExtra,
  webshot2, ggdist, spgwr, ggrepel, renv
)
