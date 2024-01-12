corr_plot <- function(dataframe, direction = NULL, show_legend = TRUE, color_name = "color", labels_facies_name = "sub", ...) {
  ORDER <- unique(dataframe$sample)
  N_PLOT <- length(ORDER)

  # Configure o layout dos plots com espaço extra para a legenda
  par(mfrow = c(1, N_PLOT + 1))  # +1 para incluir espaço para a legenda

  for (i in ORDER) {
    log_plot(data = dataframe, well = i, correlation = TRUE, y_scale = FALSE, ...)
  }

  if (show_legend && N_PLOT > 1) {
    # Posicione a legenda à direita dos gráficos, aumentando a altura da área de plotagem
    par(mar = c(5, 4, 1, 1))  # Ajuste as margens para acomodar a legenda

    # Crie uma área de plotagem vazia para a legenda
    plot.new()

    # Obtenha rótulos únicos
    unique_labels <- unique(dataframe[{{labels_facies_name}}])

    # Junte as facies2 e cores correspondentes
    reference <- dataframe %>%
      select({{color_name}}, {{labels_facies_name}})

    label_legend <- unique(reference)

    # Verifique se a legenda não está vazia e se há cores válidas
    if (nrow(label_legend) > 0 && all(is.numeric(label_legend[[1]]) || is.character(label_legend[[1]]))) {
      # Ajuste a altura da legenda para ocupar espaço vertical
      legend("right", legend = label_legend[[2]], fill = label_legend[[1]], horiz = FALSE, inset = c(0, -0.15))
    } else {
      cat("A legenda não pode ser exibida devido a valores ausentes ou incorretos na cor.\n")
    }
  }

  par(mfrow = c(1, 1))  # Restaure o layout padrão
}
