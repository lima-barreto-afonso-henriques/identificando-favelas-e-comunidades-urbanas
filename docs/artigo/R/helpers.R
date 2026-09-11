# =============================================================================
# helpers.R
# Funções de apoio para "Determinantes Socioeconômicos e Espaciais da
# Motorização na RMSP" (artigo_atualizado.qmd)
#
# Este arquivo concentra toda a lógica reutilizável de formatação de tabelas,
# citação de fonte e tema gráfico do documento. Ele é carregado uma única vez
# via `source(here::here("R", "helpers.R"))` no chunk de setup do .qmd.
#
# Manter essa lógica aqui (em vez de dentro do .qmd) tem duas vantagens para
# reprodução futura:
#   1. As funções podem ser testadas/depuradas isoladamente com `source()`,
#      sem precisar re-renderizar o documento inteiro.
#   2. Qualquer ajuste de estilo (ex.: cor do gráfico, largura de tabela) é
#      feito em um único lugar, versionado separadamente do texto do artigo.
# =============================================================================

# --- DICIONÁRIO DE PADRONIZAÇÃO (uso geral) ---
padronizar_nome_variavel <- function(vetor_nomes) {
  resultado <- case_when(
    vetor_nomes %in% c("(Intercept)", "Intercept") ~ "Constante",
    vetor_nomes == "taxa_motorizacao_automoveis" ~ "Taxa de Motorização (Auto)",
    vetor_nomes == "taxa_motorizacao_motos" ~ "Taxa de Motorização (Moto)",
    vetor_nomes == "renda_mediana_familiar" ~ "Renda Mediana Familiar (R$)",
    vetor_nomes == "log_renda_mediana_familiar" ~ "Log da Renda Mediana Familiar",
    vetor_nomes %in% c("I(log_renda_mediana_familiar * 100)", "I(log_renda_mediana_familiar * 100)") ~ "Log da Renda Mediana (× 100)",
    vetor_nomes == "tempo_medio_de_caminhada" ~ "Tempo Médio de Caminhada",
    vetor_nomes == "tempo_medio_de_viagens" ~ "Tempo Médio de Viagem",
    vetor_nomes == "densidade_hab_km2" ~ "Densidade Demográfica (hab/km²)",
    vetor_nomes == "taxa_emprego_habitante" ~ "Taxa de Emprego por Habitante",
    vetor_nomes == "taxa_matriculas_escolares" ~ "Taxa de Matrículas Escolares",
    vetor_nomes == "viagens_modo_pandemia_deixou" ~ "Viagens Não Realizadas (Pandemia)",
    vetor_nomes %in% c("rho", "rho.1") ~ "Autocorrelação Espacial (rho)",
    vetor_nomes == "lag.I(log_renda_mediana_familiar * 100)" ~ "W × Log da Renda Mediana (× 100)",
    vetor_nomes == "lag.tempo_medio_de_viagens" ~ "W × Tempo Médio de Viagem",
    # Métricas descritivas (skimr)
    vetor_nomes %in% c("mean", "numeric.mean") ~ "Média",
    vetor_nomes %in% c("sd", "numeric.sd") ~ "Desvio Padrão",
    vetor_nomes %in% c("n_missing", "n.missing") ~ "Ausentes (NAs)",
    vetor_nomes %in% c("hist", "numeric.hist") ~ "Distribuição",
    TRUE ~ stringr::str_to_title(gsub("_", " ", vetor_nomes))
  )

  # str_to_title() capitaliza cada palavra individualmente, o que distorce
  # siglas técnicas (ex.: "SAR" vira "Sar"). O bloco abaixo restaura essas
  # siglas para maiúsculas em qualquer texto já processado pela função,
  # preservando a leitura correta em todas as tabelas do documento.
  siglas <- c("Sar", "Sem", "Sdm", "Ols", "Vif", "Aic", "Gwr", "Lisa",
              "Hc3", "Ncv", "Rmsp", "Lm", "Od")
  for (sigla in siglas) {
    resultado <- gsub(paste0("\\b", sigla, "\\b"), toupper(sigla), resultado)
  }

  resultado
}

# --- FONTE PADRÃO (usada em toda tabela e figura do documento) ---
# Definida uma única vez para evitar repetir a mesma string ~19 vezes; se a
# citação da fonte precisar mudar, muda-se apenas aqui.
FONTE_OD <- "Pesquisa Origem e Destino do Metrô de São Paulo (2023) — elaboração própria."
fonte_fig <- paste("Fonte:", FONTE_OD)  # para labs(caption = ...) em gráficos ggplot

# Nota de fonte padrão para tabelas kableExtra. `threeparttable = FALSE`
# (padrão) faz a nota ocupar a largura do texto, e não a largura da
# tabela — evita a quebra de linha truncada quando a tabela é mais estreita
# do que a própria frase da nota (ex.: tabelas de 2-3 colunas).
nota_fonte <- function(tabela) {
  tabela |>
    footnote(general = FONTE_OD, general_title = "Fonte:", footnote_as_chunk = TRUE)
}

# --- RENDERIZAÇÃO PADRÃO DE TABELAS ---
# Único caminho de renderização para TODA tabela do documento. Padroniza
# font_size = 10 e full_width = TRUE (toda tabela ocupa exatamente a largura
# do texto, independentemente do número de colunas) — a uniformidade visual
# do documento inteiro depende de nenhuma tabela desviar deste caminho.
# `escalar = TRUE` soma "scale_down" para tabelas com muitas colunas, que
# de outro modo estourariam a largura da página.
renderizar_tabela <- function(df, titulo = NULL, align = NULL, escalar = FALSE) {
  latex_opts <- c("striped", "hold_position")
  if (escalar) latex_opts <- c(latex_opts, "scale_down")

  df |>
    kbl(caption = titulo, booktabs = TRUE, align = align) |>
    kable_styling(latex_options = latex_opts, font_size = 10, full_width = TRUE, position = "center") |>
    nota_fonte()
}

# --- MÉTRICAS DE AJUSTE (sempre computadas na hora a partir do modelo) ---
# Substituem o padrão anterior de variáveis soltas (r2/r2_adj/n_obs)
# recicladas entre chunks — esse padrão chegou a produzir uma nota
# desatualizada de verdade (Tabela do modelo de transporte coletivo
# reaproveitava, por engano, o R² do modelo de motorização). Chamar estas
# funções diretamente sobre o modelo, tanto na tabela quanto no texto
# corrido, torna essa classe de erro estruturalmente impossível.
r2_modelo     <- function(modelo) round(broom::glance(modelo)$r.squared, 3)
r2_adj_modelo <- function(modelo) round(broom::glance(modelo)$adj.r.squared, 3)
n_modelo      <- function(modelo) broom::glance(modelo)$nobs

nota_ajuste <- function(modelo) {
  paste0(
    "Nota: R² = ", r2_modelo(modelo),
    " | R² Ajustado = ", r2_adj_modelo(modelo),
    " | N = ", n_modelo(modelo)
  )
}

# --- 1. TABELAS GERAIS / CORRELAÇÃO ---
formatar_tabela_geral <- function(dados, titulo = NULL, escalar = FALSE) {
  df <- as.data.frame(dados)

  if (nrow(df) > 0) {
    names(df)[1] <- "Variável"
    df$Variável <- padronizar_nome_variavel(df$Variável)
  } else {
    df <- data.frame(Variável = "Nenhum dado disponível")
  }

  df |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    renderizar_tabela(titulo, escalar = escalar)
}

# --- 2. TABELAS DE MODELOS DE REGRESSÃO ---
formatar_tabela_modelo <- function(dados, titulo = NULL) {
  df <- as.data.frame(dados)

  if (nrow(df) > 0) {
    df <- df |>
      rename(
        any_of(c(
          Termo = "term",
          `Estimativa (Beta)` = "estimate",
          `Erro Padrão` = "std.error",
          `Estatística t` = "statistic",
          `p-valor` = "p.value"
        ))
      ) |>
      mutate(
        Termo = padronizar_nome_variavel(Termo),
        `p-valor` = scales::pvalue(`p-valor`, accuracy = 0.001),
        across(where(is.numeric), ~ round(.x, 3))
      )
  } else {
    df <- data.frame(
      Termo = "Nenhum dado disponível",
      `Estimativa (Beta)` = NA,
      `Erro Padrão` = NA,
      `Estatística t` = NA,
      `p-valor` = NA,
      check.names = FALSE
    )
  }

  df |> renderizar_tabela(titulo, align = c("l", "c", "c", "c", "c"))
}

# --- 3. TABELAS DESCRITIVAS (skimr / resumo) ---
formatar_tabela_artigo <- function(dados, titulo = NULL) {
  df <- as.data.frame(dados)

  if (nrow(df) > 0) {
    if ("skim_variable" %in% names(df)) {
      df <- rename(df, Variável = skim_variable)
    } else if (names(df)[1] %in% c("variable", "variável", "var")) {
      names(df)[1] <- "Variável"
    }
    df$Variável <- padronizar_nome_variavel(df$Variável)
  } else {
    df <- data.frame(Variável = "Nenhum dado disponível")
  }

  # Padroniza nomes de todas as colunas
  colnames(df) <- padronizar_nome_variavel(colnames(df))

  df <- df |>
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  if ("Dados (%)" %in% names(df)) {
    df <- df |>
      mutate(`Dados (%)` = pct_br(as.numeric(`Dados (%)`), accuracy = 0.1))
  }

  # escalar = TRUE: tabelas descritivas têm 7-8 colunas numéricas e
  # estourariam a largura da página sem o scale_down
  df |> renderizar_tabela(titulo, escalar = TRUE)
}

# --- 3b. TABELAS PEQUENAS (testes estatísticos com poucas linhas/colunas) ---
formatar_tabela_pequena <- function(dados, titulo = NULL, align = NULL) {
  dados |> renderizar_tabela(titulo, align = align)
}

# --- 3c. TESTES MULTIPLICADORES DE LAGRANGE ---
# Reaproveitada nas Seções 8 (motorização) e 9 (transporte coletivo), que
# antes repetiam o mesmo bloco de ~20 linhas trocando apenas `modelo`.
tabela_testes_lm <- function(modelo, listw) {
  nomes_didaticos <- c(
    "LMlag" = "Defasagem Espacial - Simples (Spatial Lag)",
    "RSlag" = "Defasagem Espacial - Simples (Spatial Lag)",
    "LMerr" = "Erro Espacial - Simples (Spatial Error)",
    "RSerr" = "Erro Espacial - Simples (Spatial Error)",
    "RLMlag" = "Defasagem Espacial - Robusto (Robust Lag)",
    "adjRSlag" = "Defasagem Espacial - Robusto (Robust Lag)",
    "RLMerr" = "Erro Espacial - Robusto (Robust Error)",
    "adjRSerr" = "Erro Espacial - Robusto (Robust Error)"
  )

  testes_lm <- spdep::lm.LMtests(
    modelo, listw = listw, zero.policy = TRUE,
    test = c("LMlag", "LMerr", "RLMlag", "RLMerr")
  )

  data.frame(
    `Teste / Hipótese` = unname(nomes_didaticos[names(testes_lm)]),
    `Sigla`             = names(testes_lm),
    `Estatística (LM)`  = round(sapply(testes_lm, \(x) unname(x$statistic)), 3),
    `GL`                = sapply(testes_lm, \(x) unname(x$parameter)),
    `p-valor`           = format.pval(sapply(testes_lm, \(x) x$p.value), digits = 3, eps = 0.0001),
    check.names = FALSE, row.names = NULL
  ) |>
    formatar_tabela_geral(escalar = TRUE)
}

# --- 3d. COMPARAÇÃO DE AIC ENTRE OLS E MODELOS ESPACIAIS ---
# Reaproveitada nas Seções 8 e 9. `modelos` é uma lista nomeada
# (ex.: list(`OLS Clássico` = modelo_ols, `Spatial Lag (SAR)` = modelo_lag)).
tabela_comparacao_aic <- function(modelos, incluir_loglik = FALSE) {
  tabela <- data.frame(
    Modelo = names(modelos),
    AIC = sapply(modelos, AIC)
  )
  if (incluir_loglik) {
    tabela$LogLik <- sapply(modelos, \(m) as.numeric(logLik(m)))
  }
  tabela <- tabela[order(tabela$AIC), ]
  rownames(tabela) <- NULL

  tabela |>
    mutate(across(where(is.numeric), ~ round(.x, 2))) |>
    renderizar_tabela()
}

# --- 4. TEMA GLOBAL DOS GRÁFICOS ---
# Fonte única e reduzida para todo o documento, para não competir com o
# destaque visual dos próprios gráficos. Qualquer chunk de gráfico deve
# herdar este tema automaticamente; evite adicionar `theme_minimal(...)`
# ou `theme_set(...)` dentro de um chunk específico, pois isso SUBSTITUI
# por completo o tema abaixo (theme() simples, ao contrário, apenas
# mescla/ajusta elementos pontuais e é seguro de usar).
aplicar_tema_grafico <- function() {
  theme_set(
    theme_minimal(base_size = 9) +
      theme(
        axis.text.x   = element_text(size = 8, color = "black"),
        axis.text.y   = element_text(size = 8, color = "black"),
        axis.title.x  = element_text(size = 9, margin = margin(t = 8)),
        axis.title.y  = element_text(size = 9, margin = margin(r = 8)),
        plot.title    = element_text(size = 9, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption  = element_text(size = 6, hjust = 0, color = "grey30", margin = margin(t = 6)),
        strip.text    = element_text(size = 8),
        legend.text   = element_text(size = 8),
        legend.title  = element_text(size = 8),
        legend.position = "bottom"
      )
  )

  # Padrões de ponto/linha usados em praticamente todo gráfico de dispersão e
  # de efeito marginal do documento. Fixados uma única vez aqui (antes eram
  # repetidos com update_geom_defaults() em ~6 chunks diferentes).
  update_geom_defaults("point", list(size = 2.5, color = "#2b6cb0", alpha = 0.5))
  update_geom_defaults("line",  list(color = "red", linewidth = 1))

  # Tamanho de fonte equivalente para os gráficos base R (car::qqPlot), que
  # não herdam o tema do ggplot2 e por isso precisam ser ajustados
  # separadamente para manter a mesma escala visual do restante do documento.
  par(cex.axis = 0.75, cex.lab = 0.85, cex.main = 0.85)
}

# Helper para formatar percentuais no padrão brasileiro (vírgula decimal),
# usado em todo o texto corrido para evitar mistura com o padrão "." do
# scales::percent() default, mantendo consistência com o restante do artigo.
pct_br <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",", big.mark = ".")
}

# --- 6. DIAGNÓSTICO DE RESÍDUOS (reaproveitado nos modelos simples e múltiplo) ---
tabela_shapiro <- function(modelo) {
  teste <- shapiro.test(residuals(modelo))
  tibble::tibble(
    `Estatística W` = round(teste$statistic, 4),
    `p-valor` = ifelse(teste$p.value < 0.001, "< 0.001", round(teste$p.value, 4))
  ) |>
    formatar_tabela_pequena(align = "c")
}

tabela_ncv <- function(modelo) {
  teste <- car::ncvTest(modelo)
  tibble::tibble(
    `Estatística (Qui-Quadrado)` = round(teste$ChiSquare, 3),
    `Graus de Liberdade` = teste$Df,
    `p-valor` = scales::pvalue(teste$p, accuracy = 0.001)
  ) |>
    formatar_tabela_pequena(align = c("c", "c", "c"))
}

# --- 7. GRÁFICO DE DISPERSÃO "RAINCLOUD" (Figura 1) ---
# Reaproveitado para as 5 variáveis da Figura 1 (antes ~18 linhas × 5 = ~90
# linhas de ggplot quase idêntico, variando só a variável e o rótulo do eixo).
grafico_dispersao <- function(dados, var, xlab, escala_x = NULL, caption = NULL) {
  p <- ggplot(dados, aes(x = .data[[var]], y = "Geral")) +
    stat_halfeye(
      adjust = 0.5, width = 0.6, .width = 0, justification = -0.2,
      point_colour = NA, fill = "#2b6cb0", alpha = 0.7
    ) +
    geom_boxplot(
      width = 0.12, fill = "white", color = "#1a4971",
      outlier.shape = NA, alpha = 0.8
    ) +
    geom_jitter(height = 0.05, alpha = 0.3, color = "#2b6cb0", size = 1.2) +
    labs(x = xlab, y = "", caption = caption) +
    theme(panel.grid.minor = element_blank())

  p + if (is.null(escala_x)) scale_x_continuous(limits = c(0, NA)) else escala_x
}

# --- 8. DISPERSÃO SIMPLES MOTORIZAÇÃO x RENDA (Figura 2) ---
grafico_motorizacao_vs <- function(dados, var, xlab, caption = NULL) {
  ggplot(dados, aes(x = .data[[var]], y = taxa_motorizacao_automoveis)) +
    geom_point(alpha = 0.5, size = 2.5, color = "#2b6cb0") +
    labs(y = "Taxa de Motorização (auto)", x = xlab, caption = caption)
}

# --- 9. EFEITO MARGINAL (Figuras 7 e 8) ---
# `plot_predictions()` (pacote marginaleffects) já herda os defaults globais
# de ponto/linha fixados em aplicar_tema_grafico().
grafico_efeito_marginal <- function(modelo, var, xlab,
                                     ylab = "Taxa de Motorização (auto) Predita",
                                     caption = NULL) {
  plot_predictions(modelo, condition = var, points = 0.3) +
    labs(x = xlab, y = ylab, caption = caption)
}

# --- 10. DIAGNÓSTICO GRÁFICO DE RESÍDUOS (Figuras 4, 5, 9, 10) ---
# Reaproveitados nos modelos simples e múltiplo (antes ~15 linhas × 2 cada).
grafico_residuos_hist <- function(modelo, dados) {
  dados |>
    mutate(residuos = residuals(modelo)) |>
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = after_stat(density)), fill = "lightblue") +
    geom_density() +
    labs(x = "Resíduos", y = "Densidade", caption = fonte_fig)
}

grafico_residuos_vs_ajustados <- function(modelo) {
  data.frame(
    ajustados = fitted(modelo),
    residuos = residuals(modelo, type = "pearson")
  ) |>
    ggplot(aes(x = ajustados, y = residuos)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "#2b6cb0", method = "loess") +
    labs(x = "Valores Ajustados", y = "Resíduos de Pearson", caption = fonte_fig)
}

# --- 11. MAPA CORORPLÉTICO DA GWR (Figuras 12 e 13) ---
mapa_gwr <- function(dados, fill_var, legenda, titulo, caption = NULL) {
  ggplot(dados) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", size = 0.05) +
    scale_fill_viridis_c(
      option = "magma", name = legenda,
      guide = guide_colorbar(
        direction = "horizontal", barwidth = unit(15, "cm"), barheight = unit(1, "cm"),
        title.position = "top", title.hjust = 0.5, label.position = "bottom"
      )
    ) +
    labs(title = titulo, caption = caption) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 12)
    )
}

# --- 5. CARREGAMENTO DOS DADOS (COM CACHE LOCAL) ---
# Baixa a Pesquisa OD apenas na primeira execução; nas seguintes, lê a cópia
# local em `cache_path`. Isso torna o documento reproduzível offline (depois
# do primeiro render) e evita depender da disponibilidade do GitHub a cada
# nova renderização. Para forçar um novo download (ex.: dados atualizados),
# apague o arquivo em `cache_path` ou rode com `forcar_download = TRUE`.
carregar_dados_od <- function(url, cache_path, forcar_download = FALSE) {
  if (forcar_download || !file.exists(cache_path)) {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    dados <- readRDS(url(url))
    saveRDS(dados, cache_path)
  } else {
    dados <- readRDS(cache_path)
  }
  dados
}
