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
pasta_saida <- here("outputs", "artefato")

# se necessário criação das pastas dentro do caminho do projeto
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}


# ==============================================================================
# 1. CARREGAR DADOS
# ==============================================================================
# a resolução está fixada para rodar esse
# script individualmente para cada resolução (9, 10 ou 11).
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 10

# delimita os arquivos possíveis de serem utilizados
arquivos_entrada <- c(
  "9" = "grid_hexagonal_res9.rds",
  "10" = "grid_hexagonal_artigo.rds",
  "11" = "grid_hexagonal_res11.rds"
)

# GRID HEXAGONAL -------------------------------------------------------------------
sufixo_saida <- paste0("_res", resolucao_hex)

# Defina a opção desejada: "area_pura" ou "ponderado_edificacoes"
tipo_grid <- "area_pura" # ou "ponderado_edificacoes"

# caminho do arquivo
caminho_grid_hexagonal <- file.path(
  pasta_entrada,
  paste0("grid_hexagonal_", tipo_grid, sufixo_saida, ".rds")
)

# leitura do arquivo
grid_hexagonal <-
  readRDS(caminho_grid_hexagonal)

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

# visualização e inspeção do arquivo
glimpse(grid_hexagonal)
skimr::skim(grid_hexagonal)

# Limpeza de atributos residuais de sf
# é barata e continua sendo boa prática manter o data.frame "puro" antes de
# qualquer uso com future/furrr.
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

message(
  "Tamanho de grid_df após limpeza (lobstr::obj_size): ",
  format(lobstr::obj_size(grid_df), units = "MB")
)

# visualização e inspeção do arquivo
glimpse(grid_df)
skimr::skim(grid_df)

# ==============================================================================
# 3. PARALELIZAÇÃO
# ==============================================================================
# ------------------------------------------------------------------------
# CORREÇÃO ESTRUTURAL (a causa raiz real, não os atributos de sf):
#
# O tune::fit_resamples() paraleliza os 100 FOLDS internamente via
# future.apply::future_lapply().
# Cada um dos 100 objetos `rsplit` guarda
# uma referência ao data.frame inteiro; dentro de uma sessão R isso é
# barato (copy-on-write), mas ao SERIALIZAR para os workers do future essa
# partilha se perde — cada um dos 100 splits carrega sua própria cópia dos
# dados. Resultado: ~100 x tamanho do df_model, daí os vários GiB.
#
# A solução é paralelizar no nível ERRADO->CERTO: em vez de paralelizar
# os 100 folds internos (multiplica os dados por 100), paralelizamos os 7
# LIMIARES externos (multiplica os dados por, no máximo, o nº de workers)
# e deixamos os 100 folds de cada `fit_resamples()` rodarem
# SEQUENCIALMENTE dentro de cada worker.
# ------------------------------------------------------------------------

options(parallelly.makeNodePSOCK.setup_strategy = "sequential")

# Agora que cada worker recebe só uma cópia de grid_df (~50 MB) por
# limiar (não 100 cópias), pode voltar a usar até 6 workers com folga.
n_workers <- max(1, min(6, parallelly::availableCores() - 1))

# O payload por worker agora é pequeno (um data.frame de ~50 MB por vez),
# então o limite pode voltar a um valor bem mais conservador — ele deixa
# de ser uma "rede de segurança" e volta a ser uma proteção de verdade
# contra excesso acidental de memória.
options(future.globals.maxSize = 1 * 1024^3)

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
# 4. ESPECIFICAÇÃO DO MODELO
# ==============================================================================
# DIDÁTICA:
# O modelo deve ser definido de forma "pura" e independente dos dados.
# Isso facilita a troca de algoritmos no futuro sem mexer no restante
# do código.

# Decision Tree
dt_spec <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

# ==============================================================================
# 5. FUNÇÃO DE AVALIAÇÃO MODULAR (O Jeito Tidy)
# ==============================================================================
# DIDÁTICA:
# A lógica está encapsulada em UM único limiar.
# Isso torna o código testável (você pode rodar a função para
# testar apenas o limiar de 0.5, por exemplo) e reprodutível.

avaliar_limiar <- function(limiar, data, model_spec) {
  # 5.1. Criar a variável alvo para o limiar atual
  df_model <-
    data |>
    mutate(
      is_slum = factor(
        if_else(slum_coverage_pct >= limiar, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      )
    ) |>
    limpar_atributos_sf()

  # 5.2. Criar os 100 folds
  # DIDÁTICA:
  # Fixar a seed AQUI dentro da função é fundamental.
  # Isso garante que a divisão das dobras seja exatamente a mesma para todos
  # os limiares, permitindo comparabilidade justa (pareada).
  set.seed(42)
  folds_100 <- vfold_cv(df_model, v = 100)

  # 5.3. Receita de Pré-processamento
  # (Seção 3.4 do artigo) + SMOTENC
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

    # SMOTENC para balanceamento
    step_smotenc(is_slum, over_ratio = 0.5)

  # 5.4. Workflow
  wf <-
    workflow() |>
    add_recipe(receita) |>
    add_model(model_spec)

  # 5.5. Treinamento e avaliação (100 folds)
  # CORREÇÃO: allow_par = FALSE trava a paralelização interna dos 100
  # folds, mesmo que exista um plano multisession ativo no processo (o
  # nosso, usado para paralelizar os LIMIARES externos). Isso é o que
  # evita a replicação x100 dos dados na serialização do future. Os 100
  # folds de árvore de decisão são rápidos o suficiente para rodar
  # sequencialmente sem problema.
  #
  # (Conjunto completo de métricas para bater com a Tabela A.4 do artigo:
  # Accuracy, Precision, Recall, ROC AUC, F1, + Specificity como diagnóstico extra
  # permite derivar FPR = 1 - spec, usado no texto do artigo no limiar de 70%)
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
      control = control_resamples(save_pred = FALSE, allow_par = FALSE)
    )

  # 5.6. Retorno dos resultados não sumarizados, com a coluna de limiar
  collect_metrics(
    resamples_fit,
    summarize = FALSE
  ) |>
    mutate(limiar = limiar)
}

# ==============================================================================
# 6. EXECUÇÃO DA SENSIBILIDADE (Mapeamento Funcional)
# ==============================================================================
# 6.1. Definir os limiares uma única vez para garantir consistência
# (Você pode mudar para seq(0.1, 1, by = 0.05) se quiser testar mais limiares)
limiares_teste <- seq(0.1, 0.7, by = 0.1)

# 6.2. Criar um resumo da quantidade de favelas por limiar
# Usamos a mesma variável 'limiares_teste' aqui!
distribuicao_limiares <- purrr::map_dfr(limiares_teste, function(l) {
  grid_df |>
    summarise(
      # Mantemos o número exato para servir de "chave" no join
      limiar = l,
      threshold = paste0("limiar_", l * 100),
      qtd_slums = sum(slum_coverage_pct >= l),
      total_hexagons = n(),
      pct_slums = (qtd_slums / total_hexagons) * 100
    )
})

# Visualizar a queda drástica
print(distribuicao_limiares)

# 6.3. Execução dos Modelos em Paralelo
# a paralelização acontece no nível dos 7 limiares via furrr::future_map_dfr,
# em vez de dentro de fit_resamples (nível dos 100 folds).
# O `.options = furrr_options(seed = TRUE)` garante que o set.seed(42)
# dentro de avaliar_limiar() seja respeitado de forma reprodutível em cada worker.
resultados_folds <-
  furrr::future_map_dfr(
    limiares_teste,
    ~ avaliar_limiar(
      limiar = .x,
      data = grid_df,
      model_spec = dt_spec
    ),
    .options = furrr::furrr_options(seed = TRUE)
  )

# desliga o processamento paralelo ao fim das operações
future::plan(sequential)

# 6.4. Sumarizar as métricas (tirar a média dos 100 folds) e juntar tudo
# Como 'resultados_folds' tem o resultado de cada um dos 100 folds separadamente,
# tira-se a média, transformando em colunas e juntando com a distribuição.
tabela_final <-
  resultados_folds |>
  group_by(limiar, .metric) |>
  summarise(
    media_metrica = mean(.estimate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(names_from = .metric, values_from = media_metrica) |>
  left_join(distribuicao_limiares, by = "limiar") |>
  # reorganiza as colunas
  select(threshold, limiar, qtd_slums, pct_slums, everything())

# visualizar a Tabela Completa e Pronta!
print(tabela_final)

#
# ==============================================================================
# 6. RECONSTRUÇÃO DA TABELA A.4
# ==============================================================================
# médias das métricas por limiar
tabela_A4 <-
  resultados_folds |>
  group_by(limiar, .metric) |>
  summarise(media = mean(.estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = .metric, values_from = media) |>
  mutate(
    # False Positive Rate = 1 - Specificity (diagnóstico extra)
    FPR = 1 - spec,
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

# Tabela A.4 Reconstruída
# (formato do artigo)
print(
  tabela_A4 |>
    select(-limiar)
)

# ==============================================================================
# 7. TESTE DE MANN-WHITNEY U
# ==============================================================================
# FIGURA A.12

# Isolar apenas as distribuições de Recall (100 valores por limiar)
recall_dists <-
  resultados_folds |>
  filter(.metric == "recall")

# Expandir matriz de combinações para triângulo inferior
combinacoes_mw <-
  expand_grid(t1 = limiares_teste, t2 = limiares_teste) |>
  filter(t1 > t2) |>
  # garante que seja um triângulo inferior
  rowwise() |>
  mutate(
    # Aplicar Wilcoxon Rank Sum Test (Mann-Whitney U)
    # entre as distribuições
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

# coluna de p-valor formatado no dataframe de combinações
combinacoes_mw <-
  combinacoes_mw |>
  mutate(
    p_label = if_else(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value))
  )

# Plotar a Matriz
# (Reprodução da Fig. A.12)
plot_fig_A12 <- ggplot(
  combinacoes_mw,
  aes(x = t2_label, y = t1_label, fill = significant)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  # adiciona os números (p-valores) no centro de cada azulejo
  geom_text(
    aes(label = p_label, color = significant),
    #fontface = "bold",
    size = 6
  ) +
  # controla a cor do texto para garantir contraste legível
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "white"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#a6d96a", "FALSE" = "#542788"),
    labels = c("Not Significant", "Significant"),
    name = "Statistical Significance\n(p < 0.05)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Mann-Whitney Test P-values Matrix (Lower Triangle)",
    subtitle = "Decision Tree | 100-fold CV | Recall Distributions"
  )

print(plot_fig_A12)


# ==============================================================================
# 8. SELEÇÃO DO LIMIAR: FIXO (ARTIGO, 70%) x VERIFICAÇÃO EMPÍRICA
# ==============================================================================
# O artigo escolhe 70% citando "the best trade-off, achieving a recall of 92%
# and a false positive rate of 1.18%" — ou seja, o critério declarado não é
# "recall máximo isolado", é um TRADE-OFF entre recall e FPR.
# O equivalente estatístico padrão para esse trade-off é o J de Youden
# (J = Sensibilidade + Especificidade - 1 = Recall - FPR), que soma 1 se o
# classificador for perfeito e 0 se for equivalente a chute aleatório.

tabela_A4 <-
  tabela_A4 |>
  mutate(youden_j = Recall - FPR)

limiar_artigo <-
  "limiar_70"

metricas_limiar_artigo <-
  tabela_A4 |>
  filter(Threshold == limiar_artigo) |>
  mutate(criterio = "Fixo - Artigo")

melhor_por_recall <-
  tabela_A4 |>
  slice_max(Recall, n = 1, with_ties = FALSE) |>
  mutate(criterio = "Empírico — Recall isolado")

melhor_por_trade_off <-
  tabela_A4 |>
  slice_max(youden_j, n = 1, with_ties = FALSE) |>
  mutate(criterio = "Empírico — J de Youden")

comparacao_limiares <-
  bind_rows(
    metricas_limiar_artigo,
    melhor_por_recall,
    melhor_por_trade_off
  ) |>
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

# Tabela A.4 reconstruída
# (com J de Youden)
print(
  tabela_A4 |>
    select(-limiar)
)

# Comparação:
# limiar fixo do artigo versus melhores limiares empíricos
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
    "\n>>> 
      O limiar de 70% do artigo COINCIDE com o melhor resultado empírico ",
    "nesta execução (tanto por recall quanto por trade-off). 
    Consistente com o artigo."
  )
} else {
  message(
    "\n>>> ATENÇÃO: 
      o limiar de 70% do artigo NÃO coincide com o melhor resultado ",
    "empírico nesta execução. 
    Isso pode refletir diferenças no algoritmo, na ",
    "amostra (N = ",
    nrow(grid_df),
    " hexágonos), ou variação ",
    "amostral genuína — inspecione 'comparacao_limiares' antes de decidir qual ",
    "limiar levar ao script do modelo final."
  )
}
print(metricas_limiar_artigo)


# Calcular a média e erro padrão por limiar
tradeoff_df <-
  resultados_folds |>
  filter(.metric %in% c("recall", "precision")) |>
  group_by(limiar, .metric) |>
  summarise(
    # Alterado na_rm para na.rm em ambas as linhas abaixo
    mean_val = mean(.estimate, na.rm = TRUE),
    sd_val = sd(.estimate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(limiar_pct = paste0(round(limiar * 100), "%"))

# Gráfico de Trade-off formatado para publicação científica (Padrão SBSR)
trade_off <- ggplot(
  tradeoff_df,
  aes(x = limiar, y = mean_val, color = .metric)
) +
  # Linhas verticais de destaque (10% e 70%)
  geom_vline(
    xintercept = c(0.10, 0.70),
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +

  # Séries de dados (Linhas e Pontos)
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +

  # Anotações textuais dos limiares destacados
  annotate(
    "text",
    x = 0.10,
    y = 0.95,
    label = "10% (J de Youden)",
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 4.5,
    family = "serif",
    fontface = "italic",
    color = "gray20"
  ) +
  annotate(
    "text",
    x = 0.70,
    y = 0.95,
    label = "70% (Artigo)",
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 4.5,
    family = "serif",
    fontface = "italic",
    color = "gray20"
  ) +

  # Escalas dos eixos
  scale_x_continuous(
    breaks = seq(0.1, 0.9, by = 0.1),
    labels = percent_format(accuracy = 1),
    limits = c(0.08, 0.92)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.10),
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  # Cores com contraste acessível
  scale_color_manual(
    values = c("recall" = "#2b83ba", "precision" = "#d7191c"),
    labels = c("precision" = "Precisão", "recall" = "Recall")
  ) +
  # Tema visual acadêmico (Times New Roman / Serif)
  theme_bw(base_size = 14, base_family = "serif") +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 6)),
    axis.title.y = element_text(size = 14, margin = margin(r = 6)),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = -5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3)
  ) +

  # Rótulos dos eixos
  labs(
    x = "Limiar de cobertura do rótulo",
    y = "Desempenho Médio (100-fold CV)"
  )

# Exibir o gráfico
print(trade_off)

# ==============================================================================
# 9. EXPORTAÇÃO DOS ARTEFATOS
# ==============================================================================
# Atualiza a pasta de saída para incluir a subpasta
# correspondente ao tipo de grid
pasta_saida <- file.path(here::here("outputs", "artefato", "dt"), tipo_grid)
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)


# 9.1. O "Cofre" Reprodutível (Para RMarkdown / Quarto)
artefato_artigo <- list(
  tabela_A4 = tabela_A4,
  testes_mann_whitney = combinacoes_mw,
  grafico_A12 = plot_fig_A12,
  raw_100_folds = resultados_folds,
  comparacao_limiares = comparacao_limiares,
  limiar_escolhido = melhor_por_trade_off$Threshold[1],
  tabela_final = tabela_final,
  trade_off = trade_off
)

saveRDS(
  artefato_artigo,
  file.path(pasta_saida, paste0("sensibilidade_artigo", sufixo_saida, ".rds"))
)

# 9.2. Exportação para Submissão/Leitura Humana (Arquivos Avulsos)
dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)
dir.create(file.path(pasta_saida, "figures"), showWarnings = FALSE)


# TABELAS --------------------------------------------------------------------------
# Salvar tabelas em formato aberto (CSV)
readr::write_csv(
  tabela_A4,
  file.path(pasta_saida, "tables", paste0("tabela_A4", sufixo_saida, ".csv"))
)

readr::write_csv(
  tabela_final,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_A4_final", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  comparacao_limiares,
  file.path(
    pasta_saida,
    "tables",
    paste0("comparacao_limiares", sufixo_saida, ".csv")
  )
)


# FIGURAS --------------------------------------------------------------------------
# Salvar figuras em alta resolução (ex: PDF ou PNG para a revista)
ggplot2::ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("figura_A12", sufixo_saida, ".png")
  ),
  plot = plot_fig_A12,
  width = 20, # Largura exata de 1 coluna (86 mm) no modelo SBSR
  height = 18, # Altura proporcional
  units = "cm",
  dpi = 300
)

# Salvar figuras em alta resolução (ex: PDF ou PNG para a revista)
ggplot2::ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("figura_trade_off_limiares", sufixo_saida, ".png")
  ),
  plot = trade_off,
  width = 20, # Largura exata de 1 coluna (86 mm) no modelo SBSR
  height = 18, # Altura proporcional
  units = "cm",
  dpi = 300
)

# FIM ==========================================================================
