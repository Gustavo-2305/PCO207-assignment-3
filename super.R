# ==============================================================================
# VISUALIZAÇÕES COMBINADAS PERSONALIZADAS PARA MCA (OULAD)
# ==============================================================================

library(tidyverse)
library(ggrepel)

# 1. FUNÇÕES AUXILIARES INTERNAS -----------------------------------------------

format_axis_label <- function(variance_pct, dim_index) {
  formatted_pct <- formatC(
    variance_pct,
    format = "f",
    digits = 1,
    decimal.mark = ","
  )
  sprintf("Dimensão %d (%s%%)", dim_index, formatted_pct)
}

# Extrai coordenadas fatoriais (Dim 1 e Dim 2), valida dimensões e une metadados
extract_mca_data <- function(mca_result, oulad_clean) {
  coords <- as.data.frame(mca_result$ind$coord)
  
  if (nrow(coords) != nrow(oulad_clean)) {
    stop("O número de indivíduos em `mca_result` difere de `oulad_clean`.")
  }
  
  data <- coords %>%
    rename(dim1 = `Dim 1`, dim2 = `Dim 2`) %>%
    bind_cols(oulad_clean)
  
  list(
    data = data,
    dim1_label = format_axis_label(mca_result$eig[1, 2], 1),
    dim2_label = format_axis_label(mca_result$eig[2, 2], 2)
  )
}

# Garante independência caso super.R seja carregado sem o script principal
apply_center_axes <- function(plot) {
  if (exists("add_center_axes", mode = "function")) {
    return(add_center_axes(plot))
  }
  
  built <- ggplot_build(plot)
  x_range <- built$layout$panel_params[[1]]$x.range
  y_range <- built$layout$panel_params[[1]]$y.range
  axis_arrow <- grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
  
  axis_layers <- list(
    annotate(
      "segment",
      x = x_range[1], xend = x_range[2], y = 0, yend = 0,
      color = "black", linewidth = 0.5, arrow = axis_arrow
    ),
    annotate(
      "segment",
      x = 0, xend = 0, y = y_range[1], yend = y_range[2],
      color = "black", linewidth = 0.5, arrow = axis_arrow
    )
  )
  
  plot$layers <- c(axis_layers, plot$layers)
  
  if (is.null(plot$coordinates$limits$x) && is.null(plot$coordinates$limits$y)) {
    plot <- plot + coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE)
  }
  
  plot
}

# Calcula limites adaptativos com folga para evitar cortes se eixos inverterem
compute_safe_limits <- function(data, margin = 0.25) {
  list(
    xlim = range(data$dim1, na.rm = TRUE) + c(-margin, margin),
    ylim = range(data$dim2, na.rm = TRUE) + c(-margin, margin)
  )
}

# 2. GRÁFICO 11: FAIXA ETÁRIA (55+) VS. PÓS-GRADUAÇÃO --------------------------
highest_level_and_age_band <- function(mca_result, oulad_clean) {
  mca_info <- extract_mca_data(mca_result, oulad_clean)
  
  mca_coords <- mca_info$data %>%
    mutate(
      is_age_55_plus = age_band == "55<=",
      is_postgrad = highest_education == "Post Graduate Qualification",
      highlight_group = case_when(
        is_age_55_plus & is_postgrad ~ "Ambos (55+ e Pós-graduação)",
        is_age_55_plus               ~ "55+",
        is_postgrad                  ~ "Pós-graduação",
        TRUE                         ~ "Demais alunos"
      ),
      highlight_group = factor(
        highlight_group,
        levels = c("55+", "Pós-graduação", "Ambos (55+ e Pós-graduação)", "Demais alunos")
      )
    )
  
  # Centroides calculados sobre os grupos completos (incluindo a interseção)
  centroids <- bind_rows(
    mca_coords %>%
      filter(is_age_55_plus) %>%
      summarise(dim1 = mean(dim1), dim2 = mean(dim2), group = "Centroide: 55+"),
    mca_coords %>%
      filter(is_postgrad) %>%
      summarise(dim1 = mean(dim1), dim2 = mean(dim2), group = "Centroide: Pós-graduação")
  ) %>%
    mutate(group = factor(group, levels = c("Centroide: 55+", "Centroide: Pós-graduação")))
  
  plot_overlay <- ggplot(mca_coords, aes(x = dim1, y = dim2)) +
    geom_point(
      data = filter(mca_coords, highlight_group == "Demais alunos"),
      color = "grey80", alpha = 0.15, size = 1.2
    ) +
    geom_point(
      data = filter(mca_coords, highlight_group != "Demais alunos"),
      aes(color = highlight_group), alpha = 0.65, size = 1.8
    ) +
    stat_ellipse(
      data = filter(mca_coords, is_age_55_plus),
      aes(fill = "Elipse: 55+"),
      color = "#0072B2", geom = "polygon", linetype = "solid",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    stat_ellipse(
      data = filter(mca_coords, is_postgrad),
      aes(fill = "Elipse: Pós-graduação"),
      color = "#CC79A7", geom = "polygon", linetype = "solid",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    geom_point(
      data = centroids,
      aes(shape = group),
      color = c("#0072B2", "#CC79A7"), size = 4
    ) +
    scale_color_manual(
      name = "Indivíduos Destacados",
      values = c(
        "55+" = "#0072B2",
        "Pós-graduação" = "#CC79A7",
        "Ambos (55+ e Pós-graduação)" = "#009E73"
      )
    ) +
    scale_fill_manual(
      name = "Fronteiras (68%)",
      breaks = c("Elipse: 55+", "Elipse: Pós-graduação"),
      values = c("Elipse: 55+" = "#0072B2", "Elipse: Pós-graduação" = "#CC79A7")
    ) +
    scale_shape_manual(
      name = "Centroides",
      values = c("Centroide: 55+" = 16, "Centroide: Pós-graduação" = 17)
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          color = c("#0072B2", "#CC79A7"),
          alpha = 0.2,
          linewidth = 0.9
        )
      ),
      shape = guide_legend(
        override.aes = list(color = c("#0072B2", "#CC79A7"), size = 4)
      )
    ) +
    labs(
      title = "MCA · Sobreposição entre Faixa Etária (55+) e Pós-Graduação",
      x = mca_info$dim1_label,
      y = mca_info$dim2_label
    )
  
  apply_center_axes(plot_overlay)
}

# 3. GRÁFICO 12: TRAJETÓRIAS DE FAIXA IMD VS. ESCOLARIDADE ---------------------
imd_band_and_highest_education <- function(mca_result, oulad_clean) {
  mca_info <- extract_mca_data(mca_result, oulad_clean)
  
  # Garante ordenação explícita independente do estado prévio dos fatores
  imd_levels <- c(
    "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
    "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
  )
  education_levels <- c(
    "No Formal quals", "Lower Than A Level", "A Level or Equivalent",
    "HE Qualification", "Post Graduate Qualification"
  )
  education_labels <- c(
    "No Formal quals" = "Sem qualificação formal",
    "Lower Than A Level" = "Inferior ao Ensino Médio",
    "A Level or Equivalent" = "Ensino Médio ou equivalente",
    "HE Qualification" = "Ensino Superior",
    "Post Graduate Qualification" = "Pós-graduação"
  )
  
  mca_coords <- mca_info$data %>%
    mutate(
      imd_band = factor(imd_band, levels = imd_levels),
      highest_education = factor(highest_education, levels = education_levels)
    )
  
  imd_centroids <- mca_coords %>%
    group_by(category = imd_band) %>%
    summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop") %>%
    arrange(category) %>%
    mutate(
      variable = "Faixa IMD (Socioeconômico)",
      label = ifelse(
        category %in% c("0-10%", "20-30%", "40-50%", "60-70%", "80-90%", "90-100%"),
        as.character(category),
        ""
      )
    )
  
  education_centroids <- mca_coords %>%
    group_by(category = highest_education) %>%
    summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop") %>%
    arrange(category) %>%
    mutate(
      variable = "Escolaridade",
      label = unname(education_labels[as.character(category)])
    )
  
  all_centroids <- bind_rows(imd_centroids, education_centroids)
  limits <- compute_safe_limits(all_centroids, margin = 0.65)
  
  plot_trajectories <- ggplot(mca_coords, aes(x = dim1, y = dim2)) +
    geom_point(color = "grey85", alpha = 0.08, size = 1) +
    geom_path(
      data = education_centroids, aes(group = 1),
      color = "#D55E00", linewidth = 1.4, alpha = 0.45
    ) +
    geom_path(
      data = imd_centroids, aes(group = 1),
      color = "#0072B2", linewidth = 0.9, alpha = 0.9
    ) +
    geom_point(
      data = all_centroids,
      aes(color = variable, shape = variable), size = 3.2
    ) +
    geom_text_repel(
      data = filter(imd_centroids, label != ""),
      aes(label = label),
      color = "#0072B2", size = 3.1, fontface = "bold",
      nudge_x = -0.35, hjust = 1, direction = "y",
      segment.color = "#0072B2", segment.alpha = 0.5, max.overlaps = Inf
    ) +
    geom_text_repel(
      data = education_centroids,
      aes(label = label),
      color = "#D55E00", size = 3.2, fontface = "bold",
      nudge_x = 0.35, hjust = 0, direction = "y",
      segment.color = "#D55E00", segment.alpha = 0.5, max.overlaps = Inf
    ) +
    scale_color_manual(
      name = "Variável",
      values = c(
        "Faixa IMD (Socioeconômico)" = "#0072B2",
        "Escolaridade" = "#D55E00"
      )
    ) +
    scale_shape_manual(
      name = "Variável",
      values = c(
        "Faixa IMD (Socioeconômico)" = 16,
        "Escolaridade" = 17
      )
    ) +
    coord_cartesian(xlim = limits$xlim, ylim = limits$ylim) +
    labs(
      title = "MCA · Trajetórias Ordenadas: Faixa IMD vs. Escolaridade",
      subtitle = "Centroides conectados em ordem crescente (rótulos de Faixa IMD à esquerda e de Escolaridade à direita)",
      x = mca_info$dim1_label,
      y = mca_info$dim2_label
    )
  
  apply_center_axes(plot_trajectories)
}

# 4. GRÁFICO 13: ASSOCIAÇÃO ENTRE MÓDULO E GÊNERO ------------------------------
code_module_and_gender <- function(mca_result, oulad_clean) {
  mca_info <- extract_mca_data(mca_result, oulad_clean)
  mca_coords <- mca_info$data
  
  gender_centroids <- mca_coords %>%
    group_by(gender) %>%
    summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
  
  module_centroids <- mca_coords %>%
    group_by(code_module) %>%
    summarise(
      dim1 = mean(dim1),
      dim2 = mean(dim2),
      pct_female = round(mean(gender == "F", na.rm = TRUE) * 100, 0),
      .groups = "drop"
    ) %>%
    mutate(label = paste0(code_module, " (", pct_female, "% F)"))
  
  limits <- compute_safe_limits(
    bind_rows(gender_centroids, module_centroids),
    margin = 0.85
  )
  
  plot_module_gender <- ggplot(mca_coords, aes(x = dim1, y = dim2)) +
    geom_point(aes(color = gender), alpha = 0.08, size = 1.2) +
    stat_ellipse(
      aes(color = gender, fill = gender),
      geom = "polygon", alpha = 0.14, level = 0.68, linewidth = 0.9
    ) +
    geom_point(
      data = gender_centroids,
      aes(color = gender), size = 3.5, shape = 15, show.legend = FALSE
    ) +
    geom_point(
      data = module_centroids,
      shape = 21, fill = "white", color = "black", stroke = 1.2, size = 3.5
    ) +
    geom_text_repel(
      data = module_centroids,
      aes(label = label),
      color = "black", bg.color = "white", bg.r = 0.12,
      size = 3.4, fontface = "bold", box.padding = 0.45, max.overlaps = Inf
    ) +
    scale_color_manual(
      name = "Gênero (Fronteira 68%)",
      values = c("F" = "#D55E00", "M" = "#0072B2"),
      labels = c("F" = "Feminino (F)", "M" = "Masculino (M)")
    ) +
    scale_fill_manual(
      name = "Gênero (Fronteira 68%)",
      values = c("F" = "#D55E00", "M" = "#0072B2"),
      labels = c("F" = "Feminino (F)", "M" = "Masculino (M)")
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 0.35))
    ) +
    coord_cartesian(xlim = limits$xlim, ylim = limits$ylim) +
    labs(
      title = "MCA · Associação entre Módulo e Gênero",
      subtitle = "Elipses de gênero (68%) sobrepostas aos centroides e proporção feminina de cada módulo",
      x = mca_info$dim1_label,
      y = mca_info$dim2_label
    )
  
  apply_center_axes(plot_module_gender)
}