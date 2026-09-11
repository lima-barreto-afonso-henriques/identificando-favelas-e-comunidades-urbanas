# ==============================================================================
# ANÁLISE DE SENSIBILIDADE ESPACIAL — LIMIAR PONDERADO POR EDIFICAÇÕES
# COM CHECAGEM DE MAUP (Modifiable Areal Unit Problem)
# VERSÃO CORRIGIDA: erro de "cannot allocate vector of size 21.7 Gb"
# ==============================================================================
# O QUE FOI CORRIGIDO EM RELAÇÃO AO SCRIPT ORIGINAL (leia antes de rodar):
#
# ERRO 1 — "cannot allocate vector of size 21.7 Gb" (a causa raiz):
#   53945² x 8 bytes ≈ 21,67 GB — bate exatamente com o tamanho do erro.
#   'spatialsample::spatial_block_cv()' recebia 'buffer = buffer_espacial',
#   e 'buffer_espacial' tinha o valor 0 (não NULL) sempre que a estimativa
#   de autocorrelação falhava. A função trata QUALQUER valor não-NULL de
#   'buffer' (inclusive 0) como "aplicar exclusão por buffer", etapa que
#   internamente monta uma matriz de distância DENSA ponto-a-ponto (n x n)
#   em vez de usar o índice espacial (R-tree) do sf. CORREÇÃO: só passar
#   'buffer' quando ele for estritamente positivo; caso contrário, NULL
#   (pula essa etapa inteira, sem custo de memória O(n²)).
#
# ERRO 2 — "Autocorrelação não estimada: object 'i' not found":
#   'blockCV::cv_spatial_autocor()' usa foreach internamente. Chamá-la
#   enquanto um future::plan(multisession) já está ativo, sem o adaptador
#   doFuture::registerDoFuture() registrado, faz o foreach tentar
#   paralelizar num backend mal conectado ao future — a variável de laço
#   'i' se perde na exportação para os workers. CORREÇÃO: a chamada a
#   cv_spatial_autocor() agora força 'future::plan(sequential)'
#   temporariamente (via on.exit(), que restaura o plano anterior mesmo se
#   a função falhar), isolando-a de qualquer plano paralelo ativo.
#
# SALVAGUARDA EXTRA:
#   Mesmo com os dois pontos acima corrigidos, um 'alcance' (range de
#   autocorrelação) anormalmente grande, vindo de uma estimativa ruidosa,
#   poderia gerar um buffer desproporcional e reintroduzir custo pesado.
#   Adicionado um teto (BUFFER_MAX_M) que limita o buffer espacial a um
#   valor plausível para a escala da cidade de São Paulo.
# ==============================================================================

# ==============================================================================
# 0. PACOTES E CAMINHOS
# ==============================================================================
gc()

source(here::here("scripts", "00_functions", "00_setup.R"))

pasta_entrada <- here("data", "processed")
pasta_saida <- here("outputs", "artefato")
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ==============================================================================
# 1. DADOS: CARREGAR, CORRIGIR RÓTULO PONDERADO E SELECIONAR PREDITORES
# ==============================================================================
RESOLUCAO_ORIGINAL <- 10L

grid_hexagonal <- readRDS(
  file.path(pasta_entrada, "grid_hexagonal_artigo.rds")
)
glimpse(grid_hexagonal)

grid_hexagonal <-
  grid_hexagonal |>
  # UTM — necessário para blocking/buffer em metros
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
  # filtro dasimétrico básico
  filter(number_private_residences > 0) |>
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
      # CORREÇÃO: o peso era 'number_of_buildings', que pode somar 0 num
      # grupo inteiro (hexágonos com residências no CNEFE mas nenhuma
      # edificação detectada pelo Open Buildings) — weighted.mean(w = 0)
      # devolve NaN (0/0), que virava NA em 'is_slum' e quebrava
      # step_smotenc() ("Cannot have any missing values. NAs found in
      # is_slum"), só na resolução agregada. 'number_private_residences'
      # nunca soma 0 num grupo, pois o filtro dasimétrico da Seção 1 já
      # garante number_private_residences > 0 em todo hexágono-filho —
      # também mantém a ponderação consistente com as demais variáveis
      # socioeconômicas acima.
      slum_coverage_pct = weighted.mean(
        slum_coverage_pct,
        w = number_private_residences,
        na.rm = TRUE
      ),
      media_area_pai = sum(
        avg_building_area * number_of_buildings,
        na.rm = TRUE
      ) /
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
      standard_deviation_of_building_area = sqrt(pmax(var_area_pai, 0))
    ) |>
    select(
      -n_edificacoes_total,
      -n_residencias_total,
      -media_area_pai,
      -var_area_pai
    ) |>
    rename(id_hex = id_hex_pai)

  geom_pai <- h3jsr::cell_to_polygon(unique(agregado$id_hex), simple = FALSE)
  if ("h3_address" %in% names(geom_pai) && !"id_hex" %in% names(geom_pai)) {
    geom_pai <- geom_pai |> rename(id_hex = h3_address)
  }

  agregado_sf <-
    geom_pai |>
    select(id_hex) |>
    left_join(agregado, by = "id_hex") |>
    st_set_crs(4326) |>
    st_transform(31983) |>
    # CORREÇÃO: any(as.logical(.x)) no summarise() acima devolve um
    # 'logical' puro, desfazendo o mutate(across(where(is.logical), ~
    # factor(...))) aplicado ao grid original na Seção 1. step_smotenc()
    # rejeita colunas 'logical' (exige double/integer/nominal), então sem
    # esta linha o pipeline falha em toda resolução agregada (ex.: res_9)
    # com "All columns selected for the step should be double, integer,
    # or nominal. 6 logical variables found: ...". Reaplicamos aqui o
    # MESMO padrão de factor usado no grid original, para manter os
    # tipos de coluna consistentes entre todas as resoluções testadas.
    mutate(across(
      ends_with("_intersection"),
      ~ factor(., levels = c(FALSE, TRUE))
    ))

  # CHECAGEM DEFENSIVA: falhar aqui, com mensagem clara, é bem mais fácil
  # de diagnosticar do que deixar um NA remanescente vazar até
  # step_smotenc() (a variável-resposta não passa por imputação de
  # preditores). Se isto disparar, investigue quais IDs pai têm soma de
  # 'number_private_residences' igual a zero — não deveria acontecer após
  # o filtro dasimétrico da Seção 1, mas serve de rede de segurança caso
  # o filtro mude no futuro.
  n_na_cobertura <- sum(is.na(agregado_sf$slum_coverage_pct))
  if (n_na_cobertura > 0) {
    stop(sprintf(
      paste0(
        "%d hexágono(s) na resolução %d ficaram com slum_coverage_pct ",
        "NA/NaN após a agregação — verifique grupos com soma de pesos ",
        "igual a zero."
      ),
      n_na_cobertura,
      resolucao_alvo
    ))
  }

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
    step_corr(
      all_numeric_predictors(),
      threshold = 0.90,
      method = "spearman"
    ) |>
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
N_REPETICOES <- 8

metricas_conjunto <- metric_set(
  yardstick::accuracy,
  yardstick::precision,
  yardstick::recall,
  yardstick::spec,
  yardstick::roc_auc,
  yardstick::f_meas
)

# DIDÁTICA — TETO DE SEGURANÇA PARA O BUFFER:
# Limite superior plausível para um buffer de exclusão espacial na escala
# de São Paulo (RMSP tem ~8.000 km² / diâmetro ~100 km). Um 'alcance' de
# autocorrelação estimado acima disso quase certamente reflete ruído na
# estimativa (ou uma coluna pouco informativa), não um padrão espacial
# real — usá-lo como buffer só inflaria o custo computacional sem ganho
# metodológico. Ajuste se tiver uma justificativa substantiva para outro
# valor.
BUFFER_MAX_M <- 20000 # 20 km

# DIDÁTICA — ISOLAMENTO DO cv_spatial_autocor() DE QUALQUER PLANO PARALELO:
# Função auxiliar que garante plan(sequential) apenas durante a chamada,
# restaurando o plano anterior ao final (mesmo em caso de erro), via
# on.exit(). Isso corrige o "object 'i' not found" sem exigir mudar a
# ordem de execução do restante do script.
estimar_autocorrelacao_seguro <- function(grid_pontos) {
  plano_anterior <- future::plan()
  on.exit(future::plan(plano_anterior), add = TRUE)
  future::plan(sequential)

  tryCatch(
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
}

rodar_sensibilidade_espacial <- function(grid_sf, rotulo_execucao) {
  message(sprintf("\n### Sensibilidade espacial — %s ###", rotulo_execucao))

  grid_pontos <- st_set_geometry(
    grid_sf,
    st_centroid(st_geometry(grid_sf))
  )
  grid_df <- grid_sf |> st_drop_geometry()

  # CORREÇÃO ERRO 2: chamada isolada em plano sequencial.
  diagnostico_autocorrelacao <- estimar_autocorrelacao_seguro(grid_pontos)

  v_blocos <- 10L
  buffer_espacial <- 0
  if (!is.null(diagnostico_autocorrelacao)) {
    alcance <- diagnostico_autocorrelacao$range
    area_total <- as.numeric(st_area(st_convex_hull(st_union(grid_pontos))))
    v_max_seguro <- floor(area_total / alcance^2)
    v_blocos <- max(4L, min(v_blocos, v_max_seguro, na.rm = TRUE))
    # Teto de segurança: nunca deixar o buffer estourar BUFFER_MAX_M.
    buffer_espacial <- min(alcance, BUFFER_MAX_M)
    if (alcance > BUFFER_MAX_M) {
      message(
        sprintf(
          "  [aviso] alcance estimado (%.0f m) excede o teto de segurança ",
          alcance
        ),
        sprintf("(%.0f m); truncando o buffer.", BUFFER_MAX_M)
      )
    }
  }
  message(sprintf(
    "  v_blocos = %d | buffer espacial = ~%.0f m",
    v_blocos,
    buffer_espacial
  ))

  resultados <- map_dfr(seq_len(N_REPETICOES), function(r) {
    set.seed(r)

    # CORREÇÃO ERRO 1: só passar 'buffer' quando for estritamente positivo.
    # buffer = 0 e buffer = NULL NÃO são equivalentes para
    # spatialsample::spatial_block_cv() — 0 aciona a etapa de exclusão por
    # distância (que monta a matriz densa n x n), NULL a pula inteiramente.
    blocos_sf <- if (buffer_espacial > 0) {
      spatialsample::spatial_block_cv(
        grid_pontos,
        v = v_blocos,
        buffer = buffer_espacial
      )
    } else {
      spatialsample::spatial_block_cv(
        grid_pontos,
        v = v_blocos,
        buffer = NULL
      )
    }

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

  # gc() ao final de cada resolução: os blocos_sf/rset_rotulado de 8
  # repetições x 7 limiares acumulam objetos grandes; força a liberação
  # de memória antes de seguir para a próxima resolução testada.
  gc()

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
RESOLUCOES_MAUP <- c(10L, 9L)

grids_por_resolucao <- list()
grids_por_resolucao[[as.character(RESOLUCAO_ORIGINAL)]] <- grid_hexagonal
for (r in setdiff(RESOLUCOES_MAUP, RESOLUCAO_ORIGINAL)) {
  message(sprintf(
    "\nAgregando grid da resolução %d para a resolução %d...",
    RESOLUCAO_ORIGINAL,
    r
  ))
  grids_por_resolucao[[as.character(r)]] <- agregar_para_resolucao(
    grid_hexagonal,
    r
  )
}

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
#0
resultados_maup
#
future::plan(sequential)


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

# Tabela A.4 reconstruída,
# por resolução testada
print(
  tabela_A4_todas |>
    select(-limiar),
  n = Inf
)

# ==============================================================================
# 7. CHECAGEM DE MAUP: O LIMIAR ÓTIMO MUDA ENTRE RESOLUÇÕES?
# ==============================================================================
melhor_por_execucao <-
  tabela_A4_todas |>
  group_by(execucao) |>
  slice_max(
    youden_j,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup()

# Melhor limiar (J de Youden) por resolução testada
print(
  melhor_por_execucao |>
    select(
      execucao,
      Threshold,
      Precision,
      Recall,
      FPR,
      youden_j
    )
)

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

plot_maup_recall <-
  tabela_A4_todas |>
  ggplot(aes(
    x = limiar * 100,
    y = Recall,
    color = execucao,
    group = execucao
  )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
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
    legend.position = "bottom",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12)
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
      # CORREÇÃO 1: combn(unique(Threshold), 2) sobre limiares_teste (já em
      # ordem crescente) sempre gera limiar_1 < limiar_2 — nunca o
      # contrário. O filter(as.numeric(limiar_1) > as.numeric(limiar_2))
      # abaixo, portanto, descartava as 21 linhas SEMPRE (índice de nível
      # de limiar_1 nunca é maior que o de limiar_2), deixando o gráfico
      # completamente vazio. Como combn() já devolve um único par por
      # combinação (sem diagonal, sem duplicata espelhada), não é preciso
      # filtrar nada — só mapear o eixo Y para o limiar maior do par
      # (limiar_2) e o eixo X para o menor (limiar_1), para manter a
      # leitura visual de "triângulo inferior".
      limiar_1 = factor(limiar_1, levels = niveis),
      limiar_2 = factor(limiar_2, levels = niveis),
      # CORREÇÃO 2: 'significativo' é lógico (TRUE/FALSE). Alguns
      # ambientes do ggplot2/scales não casam automaticamente os nomes
      # "TRUE"/"FALSE" de scale_fill_manual() com um vetor lógico
      # discreto, gerando o aviso "No shared levels found" e um
      # preenchimento em branco. Forçar factor de texto elimina a
      # ambiguidade de tipo.
      significativo = factor(
        as.character(significativo),
        levels = c("TRUE", "FALSE")
      )
    ) |>
    ggplot(aes(x = limiar_1, y = limiar_2, fill = significativo)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c("TRUE" = "#a6d96a", "FALSE" = "#542788"),
      labels = c("Significant", "Not Significant"),
      na.value = "grey70",
      name = "Statistical Significance\n(p < 0.05)",
      drop = FALSE
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

print(tabela_A4_todas)
print(melhor_por_execucao)
print(plot_maup_recall)
print(testes_mw_por_execucao)
print(resultados_maup)
print(RESOLUCOES_MAUP)
print(N_REPETICOES)

artefato_maup <- list(
  tabela_A4_por_resolucao = tabela_A4_todas,
  melhor_por_resolucao = melhor_por_execucao,
  grafico_maup_recall = plot_maup_recall,
  testes_mann_whitney_por_resolucao = testes_mw_por_execucao,
  raw_resultados = resultados_maup,
  resolucoes_testadas = RESOLUCOES_MAUP,
  n_repeticoes = N_REPETICOES,
  limiar_estavel_entre_resolucoes = n_distinct(melhor_por_execucao$Threshold) ==
    1
)

saveRDS(
  artefato_maup,
  file.path(pasta_saida, "sensibilidade_ponderado_edificacoes_maup.rds")
)
#
# FIM ==========================================================================
