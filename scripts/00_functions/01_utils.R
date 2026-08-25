# Funções auxiliares para evitar repetição de código
#
#
# ==============================================================================
# Funções utilitárias de uso geral (estatística e I/O), usadas principalmente
# pelo script de análise descritiva (01_eda.R). Cores e temas de ggplot2
# ficam à parte, em 02_themes.R.
# ==============================================================================

# --- Assimetria (skewness) ---------------------------------------------------
calc_skewness <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  mean((x - mean(x))^3) / (mean((x - mean(x))^2)^1.5)
}

# --- Formatação padronizada de p-valor em notação científica ----------------
formatar_pvalor <- function(p, digits = 2) {
  formatC(p, format = "e", digits = digits)
}

# --- Salvar gráfico com os parâmetros padrão do projeto ---------------------
# largura/altura/dpi padrão usados nas figuras descritivas do artigo; evita
# repetir os mesmos argumentos em cada chamada de ggsave() e garante que a
# pasta de destino exista
salvar_grafico_projeto <- function(
  plot,
  nome_arquivo,
  pasta,
  width = 14,
  height = 12,
  dpi = 300
) {
  if (!dir.exists(pasta)) {
    dir.create(pasta, recursive = TRUE)
  }
  ggsave(
    filename = file.path(pasta, nome_arquivo),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

# FIM ==========================================================================
