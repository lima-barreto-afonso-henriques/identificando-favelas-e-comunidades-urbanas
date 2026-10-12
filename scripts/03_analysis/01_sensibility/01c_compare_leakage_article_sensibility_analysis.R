# ==============================================================================
# DEMONSTRAÇÃO DE VAZAMENTO DE DADOS (DATA LEAKAGE) — SMOTENC ANTES DO SPLIT
# ==============================================================================
# OBJETIVO DESTE SCRIPT:
# Este script NÃO é uma proposta de pipeline correto. Ele reproduz, de
# propósito, um erro metodológico comum: aplicar o SMOTENC no dataset
# INTEIRO antes de gerar os folds de validação cruzada, em vez de aplicá-lo
# apenas dentro de cada fold de treino (como o script 02a/02b fazem
# corretamente via 'step_smotenc()' dentro da recipe).
#
# A frase do artigo (Apêndice A) que descreve o procedimento é ambígua:
#   "a decision tree classifier was trained using SMOTENC to address class
#    imbalance, and performance metrics were computed using 100-fold
#    cross-validation"
# — não deixa claro se o SMOTENC veio antes ou depois do split dos folds.
#
# HIPÓTESE TESTADA AQUI:
# Se o SMOTENC foi aplicado ANTES do split (neste script), duas coisas
# acontecem ao mesmo tempo:
#   1. Vazamento: exemplos sintéticos derivados de um ponto real podem cair
#      num fold diferente do ponto real que os originou, então o modelo
#      "vê" informação correlata ao ponto de validação durante o treino.
#   2. A prevalência da classe positiva no fold de VALIDAÇÃO deixa de ser
#      a prevalência real (~2-3%) e passa a refletir a proporção artificial
#      criada pelo SMOTENC (over_ratio = 0.5, ou seja, ~33% de positivos).
#      Isso por si só já infla Precision e F1, porque essas métricas são
#      sensíveis à taxa de base (base rate) da classe positiva.
#
# HIPÓTESE REFINADA
# (adicionada após comparar Decision Tree x Random Forest honestos x artigo):
# Nos dois modelos honestos (DT e RF), a Precision CAI conforme o limiar
# sobe (classe "Slum" fica mais rara) — comportamento estatisticamente
# esperado.
# Na Tabela A.4 do artigo, a Precision SOBE com o limiar — o oposto.
# Uma explicação possível: como 'over_ratio = 0.5' sempre busca
# a MESMA proporção final (~33%) de positivos, quanto mais rara for a
# classe "Slum" de partida (limiares altos), MAIS pontos sintéticos
# proporcionalmente o SMOTENC precisa gerar para chegar lá — ou seja, a
# GRAVIDADE do vazamento cresceria junto com o limiar, na direção certa
# para explicar por que a Precision do artigo sobe em vez de cair.
# A Seção 4b abaixo testa isso diretamente: para cada limiar, calcula a
# fração de pontos "Slum" que são sintéticos dentro dos folds de
# validação (não apenas a prevalência total, que já sabemos travada em
# ~33%). Se essa fração crescer do limiar 10% para o 70%, é evidência
# direta a favor dessa hipótese refinada.
#
# COMO USAR:
# Rode este script DEPOIS do 01a_article_sensibility_analysis_dt.R
# (ou do 02_article_sensibility_analysis_rf.R com Random Forest).
# Ele reaproveita a mesma estrutura de dados, os mesmos
# 100 folds (mesma seed) e o mesmo modelo, mudando SOMENTE a posição do
# balanceamento SMOTENC. No final, os artefatos são comparados lado a
# lado — se os números deste script 03 ficarem muito mais próximos da
# Tabela A.4 do artigo do que os do script 02, isso é evidência de que a
# hipótese de leakage é plausível como explicação da discrepância.
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
# 1. CARREGAR E PREPARAR DADOS (idêntico aos scripts 02a/02b)
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

# Defina a opção desejada:
# "area_pura" ou "ponderado_edificacoes"
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
  # seleção explícita de preditores — evita que colunas de metadado
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
# 2. PARALELIZAÇÃO — SEMPRE SEQUENCIAL NESTE SCRIPT (leia antes de mudar)
# ==============================================================================
# DIDÁTICA — POR QUE NÃO TEM multisession AQUI (diferente do 02/02b):
# Nos scripts 02/02b, o SMOTENC vive DENTRO da 'recipe' (step_smotenc()),
# então o objeto exportado para cada worker é só a receita — pequeno, e
# fit_resamples() consegue paralelizar pelos 100 folds com segurança.
#
# Neste script, de propósito, o SMOTENC é aplicado FORA da recipe, sobre
# o dataset inteiro, ANTES do split (é esse o comportamento problemático
# que estamos demonstrando). Isso deixa 'df_balanceado_global' um objeto
# concreto e pesado (cresce a cada limiar testado). Se um plano
# 'multisession' estiver ativo, fit_resamples() tenta serializar e enviar
# esse objeto inteiro para cada worker — e isso JÁ ESTOUROU o limite
# padrão de tamanho de globals do pacote future (953,67 MiB) na 4ª
# iteração (limiar_40) em uma execução real:
#
#   "Will not launch future due to the size of the globals 965.21 MiB
#    exceeds 953.67 MiB [...] future.globals.maxSize"
#
# Aumentar esse limite via options(future.globals.maxSize = ...) SÓ adia
# o problema e multiplica o uso de RAM por n_workers — arriscado, já que
# este projeto já bateu em estouro de memória de ~21,7 GB no script de
# MAUP (03_maup_sensibility_analysis_spatial.R) por um motivo relacionado
# (matriz densa O(n²) em vez de operação indexada). Por isso a escolha
# aqui é sempre rodar sequencial — é mais lento, mas garantidamente
# seguro em memória para este pipeline específico.
future::plan(sequential)

# ==============================================================================
# 3. ESPECIFICAÇÃO DO MODELO
# ==============================================================================
# DIDÁTICA: mantenha o MESMO modelo usado no script que você quer comparar
# (troque para rf_spec se estiver comparando com o 02b). Por padrão aqui
# usamos Decision Tree, para comparação direta e pareada com o script 02
# original.
dt_spec <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

# ==============================================================================
# 4. FUNÇÃO DE AVALIAÇÃO "COM LEAKAGE" — SMOTENC ANTES DO SPLIT
# ==============================================================================
# DIDÁTICA — DIFERENÇA-CHAVE EM RELAÇÃO AO SCRIPT 02:
# No script 02, o SMOTENC está DENTRO da 'recipe', então o tidymodels/
# recipes só o aplica sobre os dados de TREINO de cada fold (comportamento
# correto e documentado do pacote 'themis').
#
# Aqui, em vez disso, aplicamos 'themis::smotenc()' diretamente sobre
# 'df_model' completo, ANTES de chamar 'vfold_cv()'. Como consequência,
# os folds de validação cruzada passam a conter também linhas sintéticas
# (ou linhas cujos "vizinhos geradores" caíram no fold de treino), e a
# prevalência da classe positiva na validação deixa de ser a real.
#
# NOVO — MARCAÇÃO DE LINHAS SINTÉTICAS (Seção 4a) E DIAGNÓSTICO POR FOLD
# (Seção 4b): 'themis::smotenc()' preserva as linhas reais inalteradas e
# só ANEXA linhas novas (sintéticas) depois delas — mas não expõe
# nenhuma coluna indicando "isto é sintético". Para não confiar apenas
# na ORDEM das linhas (frágil, e não documentado como garantia formal
# do pacote), marcamos cada linha comparando o conteúdo de TODAS as
# colunas contra o conjunto original: se uma linha de
# 'df_balanceado_global' não existe (idêntica, em todas as colunas)
# dentro de 'df_model_imputado', ela só pode ter sido gerada pelo
# SMOTENC. Isso é uma checagem empírica (por conteúdo), não uma
# suposição sobre a implementação interna do pacote.
avaliar_limiar_com_leakage <- function(limiar, data, model_spec) {
  df_model <-
    data |>
    mutate(
      is_slum = factor(
        if_else(slum_coverage_pct >= limiar, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      )
    ) |>
    select(-slum_coverage_pct)

  # --------------------------------------------------------------------------
  # PASSO PROBLEMÁTICO: SMOTENC aplicado ANTES do split em folds.
  # 'id_hex' precisa ser removido temporariamente pois smotenc() não aceita
  # colunas de identificação — mas note que isso não resolve o problema de
  # fundo, só evita um erro técnico do pacote.
  # --------------------------------------------------------------------------
  set.seed(42)

  df_model_imputado <-
    df_model |>
    select(-id_hex) |>
    recipe(is_slum ~ .) |>
    step_impute_median(all_numeric_predictors()) |>
    step_zv(all_predictors()) |>
    prep() |>
    bake(new_data = NULL)

  df_balanceado_global <-
    themis::smotenc(
      df_model_imputado,
      var = "is_slum",
      over_ratio = 0.5
    )

  # ----------------------------------------------------------------------
  # SEÇÃO 4a — MARCAR LINHAS SINTÉTICAS POR COMPARAÇÃO DE CONTEÚDO
  # ----------------------------------------------------------------------
  # Concatena todas as colunas de cada linha num único "hash" de texto.
  # Uma linha de 'df_balanceado_global' é marcada como sintética se essa
  # combinação exata de valores não aparece em nenhuma linha de
  # 'df_model_imputado' (o conjunto 100% real, pré-SMOTENC). Com
  # variáveis contínuas de alta precisão (áreas de edificação, slope),
  # a chance de duas linhas REAIS distintas colidirem no hash é
  # desprezível — então colisão só ocorreria por coincidência estatística
  # extrema, não por design.
  hash_linha <- function(df) {
    df |>
      mutate(across(everything(), as.character)) |>
      tidyr::unite("hash_key", everything(), sep = "|") |>
      pull(hash_key)
  }

  hashes_reais <- hash_linha(df_model_imputado)

  df_balanceado_global <-
    df_balanceado_global |>
    mutate(
      is_synthetic = !hash_linha(df_balanceado_global) %in% hashes_reais
    )

  n_sintetico_total <- sum(df_balanceado_global$is_synthetic)
  message(sprintf(
    "  [limiar %.0f%%] %d de %d linhas marcadas como sintéticas (%.1f%%).",
    limiar * 100,
    n_sintetico_total,
    nrow(df_balanceado_global),
    100 * n_sintetico_total / nrow(df_balanceado_global)
  ))

  # A partir daqui, os folds são gerados sobre uma base que JÁ contém
  # exemplos sintéticos — inclusive nos conjuntos de validação.
  set.seed(42)
  folds_100_leak <-
    vfold_cv(df_balanceado_global, v = 100)

  # ----------------------------------------------------------------------
  # SEÇÃO 4b — FRAÇÃO DE PONTOS "SLUM" SINTÉTICOS POR FOLD DE VALIDAÇÃO
  # ----------------------------------------------------------------------
  # Para cada um dos 100 folds, olha só para o conjunto de AVALIAÇÃO
  # (validação) e calcula: das linhas rotuladas "Slum" ali dentro, que
  # fração é sintética? Isso é calculado ANTES de rodar qualquer modelo
  # — é uma propriedade da estrutura dos dados, não do classificador.
  fracao_sintetica_por_fold <-
    purrr::map_dbl(folds_100_leak$splits, function(split) {
      avaliacao <- rsample::assessment(split)
      slum_avaliacao <- avaliacao |> filter(is_slum == "Slum")
      if (nrow(slum_avaliacao) == 0) {
        return(NA_real_)
      }
      mean(slum_avaliacao$is_synthetic)
    })

  fracao_sintetica_media <- mean(fracao_sintetica_por_fold, na.rm = TRUE)
  message(sprintf(
    paste0(
      "  [limiar %.0f%%] fração média de 'Slum' sintético nos folds de ",
      "validação: %.1f%%."
    ),
    limiar * 100,
    100 * fracao_sintetica_media
  ))

  receita_leak <-
    recipe(is_slum ~ ., data = df_balanceado_global) |>
    # 'is_synthetic' é só um marcador diagnóstico (Seção 4a) — precisa
    # ser removido aqui para NÃO entrar como preditor no modelo (o
    # classificador não deve "aprender" a distinguir sintético de real).
    step_rm(is_synthetic) |>
    step_corr(
      all_numeric_predictors(),
      threshold = 0.90,
      method = "spearman"
    )

  wf_leak <-
    workflow() |>
    add_recipe(receita_leak) |>
    add_model(model_spec)

  resamples_fit_leak <-
    fit_resamples(
      wf_leak,
      resamples = folds_100_leak,
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

  # Registrar também a prevalência real de positivos que sobrou em cada
  # fold de validação, para deixar explícito o quanto ela se afastou dos
  # ~2-3% reais do problema original.
  prevalencia_val <-
    df_balanceado_global |>
    count(is_slum) |>
    mutate(prop = n / sum(n), limiar = limiar)

  list(
    metricas = collect_metrics(resamples_fit_leak, summarize = FALSE) |>
      mutate(limiar = limiar),
    prevalencia = prevalencia_val,
    fracao_sintetica = tibble(
      limiar = limiar,
      fracao_sintetica_slum_media = fracao_sintetica_media,
      pct_linhas_sinteticas_total = 100 *
        n_sintetico_total /
        nrow(df_balanceado_global)
    )
  )
}

# ==============================================================================
# 5. EXECUÇÃO
# ==============================================================================
limiares_teste <- seq(0.1, 0.7, by = 0.1)

resultados_leak <- purrr::map(
  limiares_teste,
  ~ avaliar_limiar_com_leakage(
    limiar = .x,
    data = grid_df,
    model_spec = dt_spec
  )
)
resultados_leak


future::plan(sequential)

resultados_folds_leak <-
  purrr::map_dfr(resultados_leak, "metricas")

prevalencia_por_limiar <-
  purrr::map_dfr(resultados_leak, "prevalencia")

fracao_sintetica_por_limiar <-
  purrr::map_dfr(resultados_leak, "fracao_sintetica")

print(prevalencia_por_limiar)
# Compare este 'prop' da classe "Slum" com 'pct_slums' de
# 'distribuicao_limiares' no script 02 — a diferença é a evidência direta
# de que a validação deixou de refletir a prevalência real.

# Fração de 'Slum' sintético nos folds de validação, por limiar

print(fracao_sintetica_por_limiar)
# LEITURA: se 'fracao_sintetica_slum_media' CRESCER do limiar_10 para o
# limiar_70, isso é evidência direta a favor da hipótese refinada: a
# gravidade do vazamento aumenta conforme a classe "Slum" real fica mais
# rara — o que explicaria por que a Precision do artigo SOBE com o
# limiar, em vez de cair como nos modelos honestos (DT e RF).

plot_fracao_sintetica <-
  fracao_sintetica_por_limiar |>
  ggplot(aes(x = limiar, y = fracao_sintetica_slum_media)) +
  geom_line(linewidth = 1, color = "#d7301f") +
  geom_point(size = 3, color = "#d7301f") +
  scale_x_continuous(
    breaks = seq(0, 0.7, by = 0.1),
    labels = percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.0, by = 0.1),
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Fração de 'Slum' sintético nos folds de validação, por limiar",
    subtitle = paste(
      "Teste da hipótese refinada: o vazamento piora conforme a classe",
      "'Slum' real fica mais rara?"
    ),
    x = "Limiar de cobertura",
    y = "Porcentagem de 'Slum' sintético na validação"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.title.y = element_text(size = 10, margin = margin(r = 6)),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3)
  )

print(plot_fracao_sintetica)

# ==============================================================================
# 6. TABELA A.4 "COM LEAKAGE" (para comparação)
# ==============================================================================
tabela_A4_leak <-
  resultados_folds_leak |>
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
  tabela_A4_leak |>
    select(-limiar)
)

# ==============================================================================
# 7. COMPARAÇÃO TRIPLA: ARTIGO x SCRIPT 02 (honesto) x SCRIPT 03 (leakage)
# ==============================================================================
tabela_artigo_A4 <- tibble::tribble(
  ~Threshold  , ~Accuracy , ~Precision , ~Recall , ~ROC_AUC , ~F1  ,
  "limiar_10" , 0.85      , 0.84       , 0.87    , 0.85     , 0.85 ,
  "limiar_20" , 0.87      , 0.86       , 0.89    , 0.87     , 0.87 ,
  "limiar_30" , 0.89      , 0.88       , 0.91    , 0.89     , 0.89 ,
  "limiar_40" , 0.90      , 0.89       , 0.92    , 0.90     , 0.90 ,
  "limiar_50" , 0.91      , 0.90       , 0.92    , 0.91     , 0.91 ,
  "limiar_60" , 0.91      , 0.91       , 0.92    , 0.91     , 0.91 ,
  "limiar_70" , 0.92      , 0.92       , 0.92    , 0.92     , 0.92
) |>
  mutate(origem = "Artigo (Tabela A.4)")

# Atualiza a pasta de saída para incluir a subpasta "leakage" e o tipo de grid
pasta_saida <- file.path(
  here::here("outputs", "artefato", "leakage"),
  tipo_grid
)

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

# Caminhos dos artefatos baseados na estrutura padronizada dos scripts 01a e 01b
caminho_artefato_02 <- file.path(
  here::here("outputs", "artefato", "dt"),
  tipo_grid,
  paste0("sensibilidade_artigo", sufixo_saida, ".rds")
)

caminho_artefato_03 <- file.path(
  here::here("outputs", "artefato", "rf"),
  tipo_grid,
  paste0("sensibilidade_artigo_rf", sufixo_saida, ".rds")
)

if (file.exists(caminho_artefato_02)) {
  artefato_02 <- readRDS(caminho_artefato_02)

  # Cria uma lista com as comparações iniciais
  tabelas_para_unir <- list(
    tabela_artigo_A4,

    artefato_02$tabela_A4 |>
      select(-limiar, -Specificity, -FPR, -any_of("youden_j")) |>
      mutate(origem = "Script 01a — honesto (SMOTENC só no treino)"),

    tabela_A4_leak |>
      select(-limiar, -Specificity, -FPR) |>
      mutate(origem = "Script 01c — com leakage (SMOTENC antes do split)")
  )

  # Verifica se o artefato RF existe e o inclui na lista
  if (file.exists(caminho_artefato_03)) {
    artefato_03 <- readRDS(caminho_artefato_03)

    tabelas_para_unir[[4]] <- artefato_03$tabela_A4 |>
      select(-limiar, -Specificity, -FPR, -any_of("youden_j")) |>
      mutate(origem = "Script 01b — Random Forest (sensibilidade_artigo_rf)")
  } else {
    message(
      "Aviso: Artefato do Random Forest (",
      paste0("sensibilidade_artigo_rf", sufixo_saida, ".rds"),
      ") não ",
      "encontrado. Pulando inclusão."
    )
  }

  # Une todas as tabelas da lista
  comparacao_multipla <- bind_rows(tabelas_para_unir) |>
    select(origem, Threshold, Accuracy, Precision, Recall, ROC_AUC, F1)

  print(comparacao_multipla, n = Inf)

  dir.create(file.path(pasta_saida, "tables"), showWarnings = FALSE)
  readr::write_csv(
    comparacao_multipla,
    file.path(
      pasta_saida,
      "tables",
      paste0("comparacao_artigo_honesto_leakage_rf", sufixo_saida, ".csv")
    )
  )
} else {
  message(
    "Artefato do script 01a não encontrado. Rode 01a_article_sensibility_dt.R ",
    "primeiro para habilitar a comparação completa."
  )
}

print(comparacao_multipla, n = Inf)

df_plot <-
  comparacao_multipla |>
  filter(origem != c("Script 01b — Random Forest (sensibilidade_artigo_rf)")) |>
  mutate(
    limiar = as.numeric(str_extract(Threshold, "\\d+")) / 100,
    origem_rotulo = case_when(
      str_detect(origem, "Artigo") ~ "(a) artigo base",
      str_detect(origem, "honesto") ~ "(b) sem vazamento de dados",
      str_detect(origem, "leakage") ~ "(c) com vazamento de dados"
    ),
    origem_rotulo = factor(
      origem_rotulo,
      levels = c(
        "(a) artigo base",
        "(b) sem vazamento de dados",
        "(c) com vazamento de dados"
      )
    )
  ) |>
  pivot_longer(
    cols = c(Precision, Recall),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = if_else(metric == "Precision", "Precisão", "Recall")
  )

# 3. Construção do gráfico comparativo em 3 painéis
grafico_comparativo <- ggplot(
  df_plot,
  aes(x = limiar, y = value, color = metric, group = metric)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~origem_rotulo, ncol = 3) +
  scale_x_continuous(
    breaks = seq(0, 0.7, by = 0.1),
    labels = percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.0, by = 0.1),
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(
    values = c("Precisão" = "#d7191c", "Recall" = "#2b83ba")
  ) +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.title.y = element_text(size = 10, margin = margin(r = 6)),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = -5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3)
  ) +
  labs(
    x = "Limiar de cobertura do rótulo",
    y = "Desempenho Médio (100-fold CV)"
  )

print(grafico_comparativo)

# ==============================================================================
# 8. EXPORTAÇÃO
# ==============================================================================
dir.create(file.path(pasta_saida, "figures"), showWarnings = FALSE)

artefato_leak <- list(
  grafico_comparativo = grafico_comparativo,
  tabela_A4_leak = tabela_A4_leak,
  raw_100_folds_leak = resultados_folds_leak,
  prevalencia_por_limiar = prevalencia_por_limiar,
  fracao_sintetica_por_limiar = fracao_sintetica_por_limiar,
  grafico_fracao_sintetica = plot_fracao_sintetica,
  comparacao_multipla = comparacao_multipla
)

saveRDS(
  artefato_leak,
  file.path(
    pasta_saida,
    paste0("sensibilidade_leakage_demo", sufixo_saida, ".rds")
  )
)


# TABELAS --------------------------------------------------------------------------
# Salvar tabelas em formato aberto (CSV)
readr::write_csv(
  tabela_A4_leak,
  file.path(
    pasta_saida,
    "tables",
    paste0("tabela_A4_leakage_demo", sufixo_saida, ".csv")
  )
)

readr::write_csv(
  fracao_sintetica_por_limiar,
  file.path(
    pasta_saida,
    "tables",
    paste0("fracao_sintetica_por_limiar", sufixo_saida, ".csv")
  )
)


# FIGURAS --------------------------------------------------------------------------
ggplot2::ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("plot_fracao_sintetica", sufixo_saida, ".png")
  ),
  plot = plot_fracao_sintetica,
  width = 20, # Largura exata de 1 coluna (86 mm) no modelo SBSR
  height = 18, # Altura proporcional
  units = "cm",
  dpi = 300
)


ggplot2::ggsave(
  filename = file.path(
    pasta_saida,
    "figures",
    paste0("grafico_comparativo", sufixo_saida, ".png")
  ),
  plot = grafico_comparativo,
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
    paste0("fracao_sintetica_por_limiar", sufixo_saida, ".png")
  ),
  plot = plot_fracao_sintetica,
  width = 20, # Largura exata de 1 coluna (86 mm) no modelo SBSR
  height = 18, # Altura proporcional
  units = "cm",
  dpi = 300
)

# ==============================================================================
# 9. LEITURA DOS RESULTADOS — O QUE PROCURAR
# ==============================================================================
# 1. Compare 'prevalencia_por_limiar$prop' (classe "Slum" nos folds deste
#    script) com 'distribuicao_limiares$pct_slums' do script 02: se a
#    prevalência nos folds deste script 03 estiver muito acima da real,
#    isso já demonstra o problema por si só, independente das métricas.
# 2. Compare as colunas Accuracy/Precision/Recall/ROC_AUC/F1 nas fontes de
#    'comparacao_multipla': se os números do script 03 (leakage) ficarem
#    visivelmente mais próximos — e mais "colados uns nos outros" — do que
#    os do script 02 (honesto) em relação à Tabela A.4 do artigo, isso é
#    evidência consistente (não prova definitiva, já que não temos acesso
#    ao código original dos autores) de que a discrepância que você
#    observou provém da ordem SMOTENC-antes-do-split, e não de um erro na
#    sua implementação.
# 3. NOVO — Olhe 'fracao_sintetica_por_limiar' e 'plot_fracao_sintetica':
#    se 'fracao_sintetica_slum_media' crescer visivelmente do limiar_10
#    para o limiar_70, isso sustenta a hipótese refinada de que a
#    gravidade do vazamento aumenta conforme a classe real fica mais
#    rara — explicando por que a Precision do artigo SOBE com o limiar
#    (padrão oposto ao observado nos modelos honestos, DT e RF).
# FIM ==========================================================================
