# ==============================================================================
# ANÁLISE DE SENSIBILIDADE ESPACIAL — LIMIAR PONDERADO POR EDIFICAÇÕES
# COM CHECAGEM DE MAUP (Modifiable Areal Unit Problem)
# ==============================================================================
# O QUE ESTE SCRIPT FAZ, EM UMA FRASE:
#   Repete a sensibilidade espacial (blocos + buffer + limiares 10%-70%) do
#   script 01_sensibilidade_ponderado_edificacoes_artigo_spatial_v2.R, mas
#   AGORA em mais de uma escala de agregação hexagonal, para checar se as
#   conclusões (limiar ótimo, recall) dependem do tamanho do hexágono
#   escolhido — a faceta de ESCALA do MAUP.
#
# O QUE É O MAUP E POR QUE IMPORTA AQUI:
#   O "Modifiable Areal Unit Problem" (Openshaw, 1984) descreve como
#   resultados estatísticos sobre dados agregados em áreas (aqui,
#   hexágonos H3) podem mudar dependendo de duas escolhas arbitrárias:
#     (a) ESCALA — o tamanho da unidade espacial (hexágono pequeno vs
#         grande);
#     (b) ZONEAMENTO — onde, especificamente, caem as fronteiras dessa
#         unidade, mesmo em um tamanho fixo.
#   O próprio artigo relata sintomas disso sem nomear o problema: a Seção
#   4.3 e a Fig. 7(b) descrevem falsos positivos causados pela "resolução
#   do polígono" incluindo áreas não-precárias dentro de um hexágono
#   predominantemente favela — um sintoma clássico de MAUP na faceta de
#   zoneamento/escala.
#   Este script ataca a faceta (a): reagregamos o MESMO grid de resolução
#   10 (a usada no artigo) para resoluções H3 mais grosseiras — usando a
#   hierarquia NATIVA pai-filho do H3, sem precisar reprocessar nenhum dado
#   bruto — e repetimos a sensibilidade em cada escala. A faceta (b)
#   (zoneamento, a um tamanho fixo) NÃO é coberta aqui: testar isso exigiria
#   deslocar/rotacionar a grade original a partir dos dados brutos, o que
#   está fora do escopo de um script de sensibilidade que só tem acesso ao
#   grid já consolidado.
#
# BASE DESTE SCRIPT:
#   01_sensibilidade_ponderado_edificacoes_artigo_spatial_v2.R (a versão
#   espacial simplificada e didática entregue anteriormente), com o mesmo
#   ajuste de caminhos/pacotes/paralelização já aplicado em
#   03_article_sensibility_analysis.R.
#
# ⚠️ CONFERIR ANTES DE RODAR — API do pacote 'h3jsr':
#   A Seção 2 usa h3jsr::get_parent() e h3jsr::cell_to_polygon(). Esses são
#   os nomes mais comuns na API atual do pacote, mas versões diferentes já
#   usaram nomes diferentes (ex.: h3_to_parent(), h3_to_geo_boundary_sf()).
#   Rode packageVersion("h3jsr") e ?h3jsr::get_parent no SEU ambiente antes
#   de confiar cegamente nesta seção — se os nomes não baterem, a mensagem
#   de erro do R vai dizer "não foi possível encontrar a função", e é só
#   trocar pelo nome equivalente da sua versão.
# ==============================================================================

# ==============================================================================
# 0. PACOTES E CAMINHOS
# ==============================================================================
gc()

source(here::here("scripts", "00_functions", "00_setup.R"))

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  here,
  tidymodels,
  themis,
  sf,
  future,
  spatialsample,
  blockCV,
  h3jsr
)

pasta_entrada <- here("data", "processed")
pasta_saida <- here("data", "outputs")
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ==============================================================================
# 1. DADOS: CARREGAR, CORRIGIR RÓTULO PONDERADO E SELECIONAR PREDITORES
# ==============================================================================
# Resolução H3 original, usada no artigo (Seção 3.3: "H3 grid at resolution
# 10, with an average hexagon area of 15,047.5 m2"). Fixado como constante
# em vez de detectado programaticamente, para não depender de uma função
# de "get resolution" cujo nome pode variar entre versões do h3jsr.
RESOLUCAO_ORIGINAL <- 10L

grid_hexagonal <- readRDS(
  file.path(pasta_entrada, "grid_hexagonal_ponderado_edificacoes_artigo.rds")
)
glimpse(grid_hexagonal)

grid_hexagonal <-
  grid_hexagonal |>
  # A definição "ponderada por edificações" vem da fração de EDIFICAÇÕES
  # (não de área) dentro de polígonos de favela. select(-any_of()) descarta
  # a versão por área primeiro (se presente), evitando colisão de nome.
  select(-any_of("slum_coverage_pct")) |>
  rename(slum_coverage_pct = slum_coverage_pct_edif) |>
  st_transform(31983) |> # UTM — necessário para blocking/buffer em metros
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
  filter(number_private_residences > 0) |> # filtro dasimétrico básico
  mutate(across(where(is.logical), ~ factor(., levels = c(FALSE, TRUE))))

message(
  "Hexágonos na resolução ",
  RESOLUCAO_ORIGINAL,
  " após filtro dasimétrico: ",
  nrow(grid_hexagonal)
)

# ==============================================================================
# 2. AGREGAÇÃO H3 PARA RESOLUÇÕES MAIS GROSSEIRAS (checagem de MAUP — escala)
# ==============================================================================
# Reagrega o grid fino (resolução 10) para uma resolução mais grosseira
# (hexágonos maiores) usando a hierarquia pai-filho nativa do H3. Cada
# variável é combinada com a regra estatisticamente correta para o seu
# tipo — não é uma média simples de médias em nenhum caso:
#   - contagens (edificações, residências): SOMA
#   - percentuais de domicílios: MÉDIA PONDERADA pelo nº de residências
#   - cobertura ponderada por edificações: MÉDIA PONDERADA pelo nº de
#     edificações (é essa a unidade que a métrica representa)
#   - média/desvio-padrão da área de edificações: decomposição de
#     variância combinada (NÃO a média das médias/SDs — isso subestimaria
#     a dispersão real)
#   - declividade média: hexágonos da MESMA resolução têm área igual entre
#     si, então a média simples entre filhos já equivale a uma média
#     ponderada por área
#   - interseções binárias (rodovia, ferrovia etc.): o hexágono-pai
#     "intersecta" se QUALQUER filho intersectar
agregar_para_resolucao <- function(grid_fino_sf, resolucao_alvo) {
  if (resolucao_alvo >= RESOLUCAO_ORIGINAL) {
    stop(
      "resolucao_alvo (",
      resolucao_alvo,
      ") precisa ser mais grosseira (número menor) que RESOLUCAO_ORIGINAL (",
      RESOLUCAO_ORIGINAL,
      ")."
    )
  }

  df <-
    grid_fino_sf |>
    st_drop_geometry() |>
    mutate(id_hex_pai = h3jsr::get_parent(id_hex, res = resolucao_alvo))

  agregado <-
    df |>
    group_by(id_hex_pai) |>
    summarise(
      n_edificacoes_total = sum(number_of_buildings, na.rm = TRUE),
      n_residencias_total = sum(number_private_residences, na.rm = TRUE),
      households_connected_to_water_supply_network = weighted.mean(
        households_connected_to_water_supply_network,
        w = number_private_residences,
        na.rm = TRUE
      ),
      households_connected_to_sewerage_system = weighted.mean(
        households_connected_to_sewerage_system,
        w = number_private_residences,
        na.rm = TRUE
      ),
      households_solid_waste_collection = weighted.mean(
        households_solid_waste_collection,
        w = number_private_residences,
        na.rm = TRUE
      ),
      households_with_sidewalk = weighted.mean(
        households_with_sidewalk,
        w = number_private_residences,
        na.rm = TRUE
      ),
      households_with_trees = weighted.mean(
        households_with_trees,
        w = number_private_residences,
        na.rm = TRUE
      ),
      slum_coverage_pct = weighted.mean(
        slum_coverage_pct,
        w = number_of_buildings,
        na.rm = TRUE
      ),
      # decomposição de variância combinada: Var_total = E[Var_i + Média_i²] - Média_total²
      media_area_pai = sum(avg_building_area * number_of_buildings, na.rm = TRUE) /
        n_edificacoes_total,
      var_area_pai = sum(
        number_of_buildings *
          (standard_deviation_of_building_area^2 + avg_building_area^2),
        na.rm = TRUE
      ) /
        n_edificacoes_total -
        media_area_pai^2,
      avg_slope_topodata = mean(avg_slope_topodata, na.rm = TRUE),
      across(ends_with("_intersection"), ~ any(as.logical(.x), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      number_of_buildings = n_edificacoes_total,
      number_private_residences = n_residencias_total,
      avg_building_area = media_area_pai,
      # pmax(..., 0) evita NaN por eventual erro de arredondamento que
      # deixe a variância combinada ligeiramente negativa
      standard_deviation_of_building_area = sqrt(pmax(var_area_pai, 0))
    ) |>
    select(
      -n_edificacoes_total,
      -n_residencias_total,
      -media_area_pai,
      -var_area_pai
    ) |>
    rename(id_hex = id_hex_pai)

  # Geometria nativa dos hexágonos-pai na resolução alvo. h3jsr devolve em
  # WGS84 (lat/lon) — reprojetamos para UTM 31983 em seguida, igual ao
  # resto do script. [CONFERIR] o nome da coluna de endereço H3 no retorno
  # de cell_to_polygon() pode variar por versão (ex.: 'h3_address', 'id').
  geom_pai <- h3jsr::cell_to_polygon(unique(agregado$id_hex), simple = FALSE)
  if ("h3_address" %in% names(geom_pai) && !"id_hex" %in% names(geom_pai)) {
    geom_pai <- geom_pai |> rename(id_hex = h3_address)
  }

  agregado_sf <-
    geom_pai |>
    select(id_hex) |>
    left_join(agregado, by = "id_hex") |>
    st_set_crs(4326) |>
    st_transform(31983)

  message(sprintf(
    "  Resolução %d: %d hexágonos (agregados de %d na resolução %d).",
    resolucao_alvo,
    nrow(agregado_sf),
    nrow(grid_fino_sf),
    RESOLUCAO_ORIGINAL
  ))

  agregado_sf
}

# ==============================================================================
# 3. RÓTULO POR LIMIAR + RECEITA (SMOTENC só entra no treino de cada fold)
# ==============================================================================
rotular_dados <- function(limiar, dados) {
  dados |>
    mutate(
      is_slum = factor(
        if_else(slum_coverage_pct >= !!limiar, "Slum", "Non_Slum"),
        levels = c("Slum", "Non_Slum")
      )
    ) |>
    select(-slum_coverage_pct)
}

criar_receita <- function(dados_rotulados) {
  recipe(is_slum ~ ., data = dados_rotulados) |>
    update_role(id_hex, new_role = "id") |>
    step_impute_median(all_numeric_predictors()) |>
    step_zv(all_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.90, method = "spearman") |>
    # step_smotenc() tem skip = TRUE por padrão: só balanceia o treino de
    # cada fold, nunca o teste — a "regra de ouro" do balanceamento em CV.
    step_smotenc(is_slum, over_ratio = 0.5)
}

remapear_splits <- function(rset_origem, novo_df) {
  novos_splits <- purrr::map(rset_origem$splits, function(s) {
    idx_in <- s$in_id
    idx_out <- setdiff(seq_len(nrow(novo_df)), idx_in)
    rsample::make_splits(
      x = list(analysis = idx_in, assessment = idx_out),
      data = novo_df
    )
  })
  rsample::manual_rset(splits = novos_splits, ids = rset_origem$id)
}

# ==============================================================================
# 4. MODELO, LIMIARES E FUNÇÃO DE SENSIBILIDADE ESPACIAL (parametrizada)
# ==============================================================================
dt_spec <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

limiares_teste <- seq(0.1, 0.7, by = 0.1)
N_REPETICOES <- 8 # reduzido de 10 para 8 porque agora é multiplicado pelo
# número de resoluções testadas (RESOLUCOES_MAUP, Seção 5)

metricas_conjunto <- metric_set(
  yardstick::accuracy,
  yardstick::precision,
  yardstick::recall,
  yardstick::spec,
  yardstick::roc_auc,
  yardstick::f_meas
)

# Roda a sensibilidade espacial completa (blocos + buffer + limiares x
# repetições) para UM grid de entrada. É a mesma lógica de
# 01_sensibilidade_ponderado_edificacoes_artigo_spatial_v2.R, só que
# encapsulada em função para poder ser chamada uma vez por resolução.
rodar_sensibilidade_espacial <- function(grid_sf, rotulo_execucao) {
  message(sprintf("\n### Sensibilidade espacial — %s ###", rotulo_execucao))

  grid_pontos <- st_set_geometry(
    grid_sf,
    st_centroid(st_geometry(grid_sf))
  )
  grid_df <- grid_sf |> st_drop_geometry()

  diagnostico_autocorrelacao <- tryCatch(
    blockCV::cv_spatial_autocor(
      grid_pontos,
      column = "number_of_buildings",
      plot = FALSE
    ),
    error = function(e) {
      message("  Autocorrelação não estimada: ", conditionMessage(e))
      NULL
    }
  )

  v_blocos <- 10L
  buffer_espacial <- 0
  if (!is.null(diagnostico_autocorrelacao)) {
    alcance <- diagnostico_autocorrelacao$range
    area_total <- as.numeric(st_area(st_convex_hull(st_union(grid_pontos))))
    v_max_seguro <- floor(area_total / alcance^2)
    v_blocos <- max(4L, min(v_blocos, v_max_seguro, na.rm = TRUE))
    buffer_espacial <- alcance
  }
  message(sprintf(
    "  v_blocos = %d | buffer espacial = ~%.0f m",
    v_blocos,
    buffer_espacial
  ))

  resultados <- map_dfr(seq_len(N_REPETICOES), function(r) {
    set.seed(r)
    blocos_sf <- spatialsample::spatial_block_cv(
      grid_pontos,
      v = v_blocos,
      buffer = buffer_espacial
    )

    map_dfr(limiares_teste, function(limiar) {
      dados_rotulados <- rotular_dados(limiar, grid_df)
      rset_rotulado <- remapear_splits(blocos_sf, dados_rotulados)

      wf <-
        workflow() |>
        add_recipe(criar_receita(dados_rotulados)) |>
        add_model(dt_spec)

      resultado <- tryCatch(
        fit_resamples(
          wf,
          resamples = rset_rotulado,
          metrics = metricas_conjunto,
          control = control_resamples(save_pred = FALSE)
        ),
        error = function(e) {
          message(sprintf(
            "    [%s | limiar %.0f%%] falhou nesta repetição: %s",
            rotulo_execucao,
            limiar * 100,
            conditionMessage(e)
          ))
          NULL
        }
      )
      if (is.null(resultado)) {
        return(tibble())
      }
      collect_metrics(resultado, summarize = FALSE) |>
        mutate(limiar = limiar, repeticao = r)
    })
  })

  resultados |>
    mutate(
      execucao = rotulo_execucao,
      v_blocos = v_blocos,
      buffer_m = buffer_espacial
    )
}

# ==============================================================================
# 5. RODAR A SENSIBILIDADE EM CADA RESOLUÇÃO (o CERNE da checagem de MAUP)
# ==============================================================================
# 10 = resolução original do artigo (baseline). 9 = uma escala mais
# grosseira (hexágonos H3 ~7x maiores em área) para testar sensibilidade.
# Acrescente 8L ao vetor se quiser um terceiro ponto (hexágonos ~49x
# maiores) — o custo computacional cresce proporcionalmente ao nº de
# resoluções testadas.
RESOLUCOES_MAUP <- c(10L, 9L)

grids_por_resolucao <- list()
grids_por_resolucao[[as.character(RESOLUCAO_ORIGINAL)]] <- grid_hexagonal
for (r in setdiff(RESOLUCOES_MAUP, RESOLUCAO_ORIGINAL)) {
  message(sprintf("\nAgregando grid da resolução %d para a resolução %d...", RESOLUCAO_ORIGINAL, r))
  grids_por_resolucao[[as.character(r)]] <- agregar_para_resolucao(grid_hexagonal, r)
}

# Paralelização com fallback seguro (mesma correção do script não-espacial):
# sobe os workers um de cada vez para evitar a rajada de conexões
# simultâneas barrada por firewall/VPN/antivírus; se falhar mesmo assim,
# roda sequencialmente em vez de travar.
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

resultados_maup <- map_dfr(names(grids_por_resolucao), function(res_label) {
  rodar_sensibilidade_espacial(
    grids_por_resolucao[[res_label]],
    rotulo_execucao = paste0("res_", res_label)
  )
})
#
future::plan(sequential)
#
resultados_maup

# ==============================================================================
# 6. TABELA A.4 POR RESOLUÇÃO (médias por limiar x execução)
# ==============================================================================
tabela_A4_todas <-
  resultados_maup |>
  group_by(execucao, limiar, .metric) |>
  summarise(media = mean(.estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = .metric, values_from = media) |>
  mutate(
    FPR = 1 - spec,
    Threshold = paste0("limiar_", round(limiar * 100)),
    youden_j = recall - FPR
  ) |>
  arrange(execucao, limiar) |>
  rename(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    ROC_AUC = roc_auc,
    F1 = f_meas,
    Specificity = spec
  ) |>
  select(
    execucao,
    Threshold,
    limiar,
    Accuracy,
    Precision,
    Recall,
    ROC_AUC,
    F1,
    Specificity,
    FPR,
    youden_j
  )

message("\n=== Tabela A.4 reconstruída, por resolução testada ===")
print(tabela_A4_todas |> select(-limiar))

# ==============================================================================
# 7. CHECAGEM DE MAUP: O LIMIAR ÓTIMO MUDA ENTRE RESOLUÇÕES?
# ==============================================================================
melhor_por_execucao <-
  tabela_A4_todas |>
  group_by(execucao) |>
  slice_max(youden_j, n = 1, with_ties = FALSE) |>
  ungroup()

message("\n=== Melhor limiar (J de Youden) por resolução testada ===")
print(melhor_por_execucao |> select(execucao, Threshold, Recall, FPR, youden_j))

if (n_distinct(melhor_por_execucao$Threshold) == 1) {
  message(
    "\n>>> O limiar ótimo (J de Youden) é O MESMO em todas as resoluções ",
    "testadas — evidência de que a escolha do limiar não é um artefato do ",
    "tamanho do hexágono (faceta de ESCALA do MAUP)."
  )
} else {
  message(
    "\n>>> ATENÇÃO: o limiar ótimo MUDA entre resoluções — evidência de ",
    "sensibilidade ao MAUP (efeito de escala). Reporte isso explicitamente ",
    "ao interpretar os resultados; considere fixar a escala de análise com ",
    "base em uma justificativa substantiva (ex.: a resolução usada no ",
    "artigo, para comparabilidade direta), e não apenas na resolução que ",
    "maximiza uma métrica."
  )
}

# Gráfico comparativo: Recall x limiar, uma linha por resolução — a forma
# mais direta de ver, visualmente, se a curva de sensibilidade se desloca
# quando o tamanho do hexágono muda.
plot_maup_recall <-
  tabela_A4_todas |>
  ggplot(aes(x = limiar * 100, y = Recall, color = execucao, group = execucao)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 70, 10)) +
  labs(
    title = "Recall x limiar de cobertura, por resolução H3",
    subtitle = "Checagem de sensibilidade ao MAUP (faceta de escala)",
    x = "Limiar de cobertura (%)",
    y = "Recall médio (CV espacial)",
    color = "Resolução"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_maup_recall)

# ==============================================================================
# 8. TESTE DE MANN-WHITNEY U E FIGURA A.12 (por resolução)
# ==============================================================================
rodar_mann_whitney <- function(dados_execucao) {
  recall_por_avaliacao <-
    dados_execucao |>
    filter(.metric == "recall") |>
    mutate(Threshold = paste0("limiar_", round(limiar * 100)))

  pares_limiares <- combn(
    unique(recall_por_avaliacao$Threshold),
    2,
    simplify = FALSE
  )

  map_dfr(pares_limiares, function(par) {
    grupo_a <- recall_por_avaliacao |>
      filter(Threshold == par[1]) |>
      pull(.estimate)
    grupo_b <- recall_por_avaliacao |>
      filter(Threshold == par[2]) |>
      pull(.estimate)
    teste <- suppressWarnings(wilcox.test(grupo_a, grupo_b, exact = FALSE))
    tibble(
      limiar_1 = par[1],
      limiar_2 = par[2],
      p_value = teste$p.value,
      significativo = teste$p.value < 0.05
    )
  })
}

plotar_matriz_mw <- function(teste_mw, subtitulo) {
  niveis <- paste0("limiar_", seq(10, 70, 10))
  teste_mw |>
    mutate(
      limiar_1 = factor(limiar_1, levels = niveis),
      limiar_2 = factor(limiar_2, levels = niveis)
    ) |>
    filter(as.numeric(limiar_1) > as.numeric(limiar_2)) |>
    ggplot(aes(x = limiar_2, y = limiar_1, fill = significativo)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c("TRUE" = "#a6d96a", "FALSE" = "#542788"),
      labels = c("Significant", "Not Significant"),
      na.value = "grey70",
      name = "Statistical Significance\n(p < 0.05)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = "Mann-Whitney Test P-values Matrix (Lower Triangle)",
      subtitle = subtitulo
    )
}

testes_mw_por_execucao <- map(names(grids_por_resolucao), function(res_label) {
  execucao_label <- paste0("res_", res_label)
  rodar_mann_whitney(resultados_maup |> filter(execucao == execucao_label))
})
names(testes_mw_por_execucao) <- paste0("res_", names(grids_por_resolucao))

iwalk(testes_mw_por_execucao, function(teste_mw, nome_execucao) {
  print(plotar_matriz_mw(
    teste_mw,
    sprintf("Decision Tree | CV espacial | Recall — %s", nome_execucao)
  ))
})

# ==============================================================================
# 9. EXPORTAÇÃO DOS ARTEFATOS
# ==============================================================================
artefato_maup <- list(
  tabela_A4_por_resolucao = tabela_A4_todas,
  melhor_por_resolucao = melhor_por_execucao,
  grafico_maup_recall = plot_maup_recall,
  testes_mann_whitney_por_resolucao = testes_mw_por_execucao,
  raw_resultados = resultados_maup,
  resolucoes_testadas = RESOLUCOES_MAUP,
  n_repeticoes = N_REPETICOES,
  limiar_estavel_entre_resolucoes = n_distinct(melhor_por_execucao$Threshold) == 1
)

saveRDS(
  artefato_maup,
  file.path(pasta_saida, "sensibilidade_ponderado_edificacoes_maup.rds")
)

message(
  "\n>>> Checagem de MAUP (faceta de escala) concluída e salva em '",
  file.path(pasta_saida, "sensibilidade_ponderado_edificacoes_maup.rds"),
  "'."
)

# FIM ==========================================================================
