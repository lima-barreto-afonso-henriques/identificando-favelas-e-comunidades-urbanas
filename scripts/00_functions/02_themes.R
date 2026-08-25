# Temas customizados do ggplot2
#
#
# ==============================================================================
# Centraliza a paleta de cores e os temas usados nos gráficos descritivos do
# projeto (ver 01_eda.R), para manter consistência visual entre as figuras e
# evitar repetir o mesmo bloco de theme() em cada função de plotagem.
# ==============================================================================

# --- Paleta de cores do projeto ----------------------------------------------
cores_projeto <- c("Non Slum" = "#4A154B", "Slum" = "#F2C724")

# cores do gráfico de densidade global (Fig. B.13), sem separação por classe
cor_densidade_global_fill <- "#ceddf0"
cor_densidade_global_linha <- "#7fa9d9"

# gradiente do heatmap de correlação de Spearman (Fig. 3): azul (negativo) ->
# branco (zero) -> vermelho (positivo)
cores_correlacao <- c("#3c78d8", "white", "#e06666")

# --- Escalas de cor prontas, usando a paleta do projeto ----------------------
escala_fill_projeto <- function(...) {
  scale_fill_manual(values = cores_projeto, ...)
}

escala_color_projeto <- function(...) {
  scale_color_manual(values = cores_projeto, ...)
}

# --- Tema base compartilhado --------------------------------------------------
# elementos comuns a todos os gráficos descritivos: fundo minimalista sem
# grid, eixos com linha preta, legenda embaixo sem título, texto do eixo Y
# em negrito
tema_projeto_base <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.line.x = element_line(color = "black", linewidth = 0.5)
    )
}

# tema dos boxplots por classe (Fig. 2): sem rótulos no eixo X (a classe já
# aparece via cor/legenda) e com subtítulo (caption) do teste t
tema_projeto_boxplot <- function(base_size = 12) {
  tema_projeto_base(base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption = element_text(
        hjust = 0.5,
        size = 12,
        color = "black",
        lineheight = 1.2
      ),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# tema dos gráficos de densidade (global e por classe): eixo X visível,
# título um pouco menor que o dos boxplots. `com_caption = FALSE` remove o
# subtítulo para gráficos que não usam essa informação (ex.: densidade por
# classe, que não tem teste associado)
tema_projeto_densidade <- function(base_size = 12, com_caption = TRUE) {
  tema <- tema_projeto_base(base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.ticks.x = element_blank()
    )

  if (com_caption) {
    tema <- tema +
      theme(
        plot.caption = element_text(
          hjust = 0.5,
          size = 12,
          color = "black",
          lineheight = 1.2
        )
      )
  }

  tema
}

# tema do heatmap de correlação de Spearman (Fig. 3)
tema_projeto_corr <- function() {
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 14)
  )
}

# tema aplicado com "&" ao combinar gráficos com patchwork (wrap_plots +
# plot_layout(guides = "collect")), para uniformizar a legenda do grid
tema_projeto_legenda_grid <- function() {
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold")
  )
}

# FIM ==========================================================================
