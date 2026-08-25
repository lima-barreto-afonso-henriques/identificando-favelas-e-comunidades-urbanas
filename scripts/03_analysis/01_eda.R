# Exploração visual e estatística descritiva

# ==============================================================================
# GRÁFICOS DESCRITIVOS DO ARTIGO (BOXPLOTS, DENSIDADES, CORRELAÇÃO)
# Objetivo: carregar o dataset hexagonal já consolidado (script 13),
# reproduzir a divisão treino/teste com bloqueio espacial, e gerar os
# gráficos descritivos equivalentes às Fig. 2 (boxplots), Fig. B.13
# (densidade/normalidade) e Fig. 3 (correlação de Spearman) do artigo.
#
# A consolidação das bases (join por id_hex) NÃO acontece neste script —
# ela tem um script próprio ('13_consolidar_grid_hexagonal_area_pura.R'),
# com checagem de duplicatas e validação dedicadas, para não duplicar essa
# lógica em dois lugares do pipeline.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# funções utilitárias e temas/paleta de cores compartilhados entre os
# gráficos do projeto (ajustar o caminho abaixo se estes arquivos morarem em
# outra pasta do repositório)
source(here::here("scripts", "00_functions", "01_utils.R"))
source(here::here("scripts", "00_functions", "02_themes.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. CARREGAR O DATASET HEXAGONAL JÁ CONSOLIDADO
# ==============================================================================
# a consolidação (join de todas as bases por id_hex, com checagem de
# duplicatas e validação final) é feita no script 13 — este script de
# gráficos só CONSOME o resultado, para não duplicar a mesma lógica em
# dois lugares do pipeline. Se o arquivo ainda não existir, rodamos o
# script 13 automaticamente (mesmo padrão já usado no script 11 de
# comparação de métodos dasimétricos).

arquivo_grid_hexagonal <-
  file.path(pasta_saida, "grid_hexagonal_artigo.rds")

if (!file.exists(arquivo_grid_hexagonal)) {
  message(
    ">>> '",
    basename(arquivo_grid_hexagonal),
    "' não encontrado — rodando o script de consolidação..."
  )
  source(here("scripts", "13_consolidar_grid_hexagonal_area_pura.R"))
}

grid_hexagonal <-
  readRDS(arquivo_grid_hexagonal)
glimpse(grid_hexagonal)


# ==============================================================================
# 2. PREPARAÇÃO INICIAL E DIVISÃO TREINO/TESTE COM BLOQUEIO ESPACIAL
# ==============================================================================
# usa bloqueio espacial (não uma amostra aleatória simples) para o holdout:
# hexágonos vizinhos tendem a ser parecidos (autocorrelação espacial), então
# uma divisão aleatória "vaza" informação do treino para o teste. O
# bloqueio espacial agrupa hexágonos próximos no mesmo lado da divisão

grid_preparada <-
  grid_hexagonal |>
  mutate(.row = row_number()) |>
  mutate(is_slum = factor(is_slum, levels = c("1", "0"))) |>
  mutate(across(where(is.logical), as.numeric))

set.seed(42)
# v = 5 -> cada bloco representa ~20% dos dados; usamos 1 bloco como
# holdout, mantendo a proporção 80/20 do desenho original (Seção 3.5)
blocos_holdout <- spatial_block_cv(grid_preparada, v = 5)
divisao_inicial <- blocos_holdout$splits[[1]]

train_data <- analysis(divisao_inicial)
test_data <- assessment(divisao_inicial)


# ==============================================================================
# 4. ANÁLISE DESCRITIVA (ALINHADA AO ARTIGO — SEÇÃO 3.4 E 4.1)
# ==============================================================================

# --- 4.1 Assimetria (skewness): função utilitária movida para 01_utils.R --
# calc_skewness() agora é carregada via source() no topo deste script

# --- 4.2 Preparar os dados descritivos (apenas o conjunto de TREINO) ------
# usar só o treino evita que a análise descritiva "espie" o conjunto de
# teste antes da validação do modelo — mesma lógica de higiene metodológica
# de nunca ajustar decisões de pré-processamento olhando para o holdout
dados_descritivos <-
  train_data |>
  st_drop_geometry() |>
  select(-id_hex, -slum_coverage_pct, -.row) |>
  select(-contains("_intersection")) |>
  select(where(~ !is.numeric(.x) || sd(.x, na.rm = TRUE) > 0))

dados_longos <-
  dados_descritivos |>
  mutate(
    is_slum_label = factor(
      is_slum,
      levels = c("0", "1"),
      labels = c("Non Slum", "Slum")
    )
  ) |>
  select(
    is_slum_label,
    `Households connected to water supply network (%)` = households_connected_to_water_supply_network,
    `Households connected to sewerage system (%)` = households_connected_to_sewerage_system,
    `Households solid waste collection (%)` = households_solid_waste_collection,
    `Households with sidewalk (%)` = households_with_sidewalk,
    `Households with trees (%)` = households_with_trees,
    `Number of residences` = number_private_residences,
    `Number of buildings` = number_of_buildings,
    `Standard deviation of building area` = standard_deviation_of_building_area,
    `Average building area` = avg_building_area,
    `Average slope` = avg_slope_topodata
  ) |>
  pivot_longer(
    cols = -is_slum_label,
    names_to = "variavel",
    values_to = "valor"
  ) |>
  mutate(variavel = factor(variavel, levels = unique(variavel)))

lista_vars <- levels(dados_longos$variavel)


# --- 4.3 Boxplots por classe (equivalente à Fig. 2 do artigo) -------------

criar_boxplot <- function(nome_var) {
  df_sub <- dados_longos |> filter(variavel == nome_var)
  teste <- t.test(valor ~ is_slum_label, data = df_sub)

  subtitulo <- sprintf(
    "t-statistic = %.2f, p-value = %s\nSlum and Non Slum distributions are different",
    teste$statistic,
    formatar_pvalor(teste$p.value, digits = 2)
  )

  ggplot(df_sub, aes(x = is_slum_label, y = valor, fill = is_slum_label)) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.2,
      color = "black",
      linewidth = 0.5
    ) +
    geom_boxplot(
      outlier.size = 1.2,
      outlier.color = "black",
      width = 0.6,
      lwd = 0.4
    ) +
    labs(title = nome_var, caption = subtitulo, x = NULL, y = NULL) +
    escala_fill_projeto() +
    tema_projeto_boxplot()
}

grid_boxplots <-
  wrap_plots(lapply(lista_vars, criar_boxplot), ncol = 3) +
  plot_layout(guides = "collect") &
  tema_projeto_legenda_grid()


# --- 4.4 Densidade global, com teste de normalidade (Fig. B.13) -----------

criar_density_global <- function(nome_var) {
  df_sub <- dados_longos |> filter(variavel == nome_var)
  skew_val <- calc_skewness(df_sub$valor)
  skew_dir <- ifelse(skew_val > 0, "Positive", "Negative")

  # o teste de Shapiro-Wilk aceita no máximo 5.000 observações — amostramos
  # quando o dataset é maior, mantendo a reprodutibilidade via seed fixa
  set.seed(42)
  shapiro_p <- shapiro.test(sample(
    df_sub$valor,
    min(4999, length(df_sub$valor))
  ))$p.value
  norm_text <- ifelse(shapiro_p > 0.05, "Normal", "Not Normal")

  subtitulo <- sprintf(
    "Skewness: %.2f (%s)\nShapiro: p=%s (%s)",
    skew_val,
    skew_dir,
    formatar_pvalor(shapiro_p, digits = 4),
    norm_text
  )

  ggplot(df_sub, aes(x = valor)) +
    geom_density(
      fill = cor_densidade_global_fill,
      color = cor_densidade_global_linha,
      alpha = 0.7,
      linewidth = 0.5
    ) +
    labs(
      title = paste("Distribution of", nome_var),
      caption = subtitulo,
      x = NULL,
      y = "Density"
    ) +
    tema_projeto_densidade()
}

grid_densidades_globais <- wrap_plots(
  lapply(lista_vars, criar_density_global),
  ncol = 3
)


# --- 4.5 Densidade por classe (Slum vs. Non Slum sobrepostos) -------------

criar_density_classes <- function(nome_var) {
  df_sub <- dados_longos |> filter(variavel == nome_var)

  ggplot(df_sub, aes(x = valor, fill = is_slum_label, color = is_slum_label)) +
    geom_density(alpha = 0.35, linewidth = 0.4) +
    labs(title = paste("Density of", nome_var), x = NULL, y = "Density") +
    escala_fill_projeto(name = "Classes") +
    escala_color_projeto(name = "Classes") +
    tema_projeto_densidade(com_caption = FALSE)
}

grid_densidades_classes <-
  wrap_plots(lapply(lista_vars, criar_density_classes), ncol = 3) +
  plot_layout(guides = "collect") &
  tema_projeto_legenda_grid()


# --- 4.6 Correlação de Spearman entre variáveis contínuas (Fig. 3) --------
# usada para identificar pares de variáveis redundantes (coeficiente > 0.9)
# antes da modelagem — critério de corte descrito na Seção 3.4 do artigo

dados_descritivos_renomeados <-
  dados_descritivos |>
  select(
    `Households connected to water supply network (%)` = households_connected_to_water_supply_network,
    `Households connected to sewerage system (%)` = households_connected_to_sewerage_system,
    `Households solid waste collection (%)` = households_solid_waste_collection,
    `Households with sidewalk (%)` = households_with_sidewalk,
    `Households with trees (%)` = households_with_trees,
    `Number of residences` = number_private_residences,
    `Number of buildings` = number_of_buildings,
    `Standard deviation of building area` = standard_deviation_of_building_area,
    `Average building area` = avg_building_area,
    `Average slope` = avg_slope_topodata
  )

matriz_cor_final <-
  dados_descritivos_renomeados |>
  select(where(is.numeric)) |>
  cor(use = "complete.obs", method = "spearman")

ordem_original <- colnames(matriz_cor_final)

# checagem automática: sinaliza pares acima do limiar de redundância antes
# de qualquer decisão manual de remoção de variáveis
limiar_correlacao <- 0.9
pares_altos <- which(
  abs(matriz_cor_final) > limiar_correlacao & upper.tri(matriz_cor_final),
  arr.ind = TRUE
)
if (nrow(pares_altos) > 0) {
  for (i in seq_len(nrow(pares_altos))) {
    var_a <- rownames(matriz_cor_final)[pares_altos[i, 1]]
    var_b <- colnames(matriz_cor_final)[pares_altos[i, 2]]
    message(sprintf(
      "Correlação alta (> %.1f): '%s' x '%s' = %.2f — considerar remover uma das duas antes da modelagem.",
      limiar_correlacao,
      var_a,
      var_b,
      matriz_cor_final[pares_altos[i, 1], pares_altos[i, 2]]
    ))
  }
} else {
  message(sprintf(
    "Nenhum par de variáveis excede o limiar de correlação de %.1f.",
    limiar_correlacao
  ))
}

plot_corr <-
  ggcorrplot(
    matriz_cor_final,
    hc.order = FALSE,
    type = "upper",
    outline.col = "white",
    colors = cores_correlacao,
    title = "",
    lab = TRUE,
    lab_size = 5
  ) +
  scale_x_discrete(limits = ordem_original) +
  scale_y_discrete(limits = rev(ordem_original)) +
  guides(
    fill = guide_colorbar(
      barheight = unit(18, "cm"),
      barwidth = unit(1, "cm"),
      ticks = TRUE,
      title = NULL
    )
  ) +
  tema_projeto_corr()


# ==============================================================================
# 5. EXIBIR E SALVAR OS GRÁFICOS
# ==============================================================================

print(grid_boxplots)
print(grid_densidades_globais)
print(grid_densidades_classes)
print(plot_corr)

pasta_figuras <- here("outputs", "figures")

# largura/altura/dpi padrão (14x12, 300dpi) usadas nas figuras em grid;
# plot_corr usa dimensões próprias (10x9) — ver salvar_grafico_projeto()
# em 01_utils.R
salvar_grafico_projeto(
  grid_boxplots,
  "fig02_boxplots_slum_non_slum.png",
  pasta_figuras
)
salvar_grafico_projeto(
  grid_densidades_globais,
  "figB13_densidade_normalidade.png",
  pasta_figuras
)
salvar_grafico_projeto(
  grid_densidades_classes,
  "fig_densidade_por_classe.png",
  pasta_figuras
)
salvar_grafico_projeto(
  plot_corr,
  "fig03_correlacao_spearman.png",
  pasta_figuras,
  width = 10,
  height = 9
)

# FIM ==========================================================================
