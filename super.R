# ==============================================================================
# VISUALIZAÇÕES COMBINADAS PERSONALIZADAS PARA MCA (OULAD)
# ==============================================================================

library(tidyverse)
library(ggrepel)

# 1. FUNÇÕES AUXILIARES INTERNAS -----------------------------------------------

# Extrai coordenadas fatoriais (Dim 1 e Dim 2), valida dimensões e une metadados
extrair_dados_mca <- function(mca_result, oulad_clean) {
  coords <- as.data.frame(mca_result$ind$coord)
  
  if (nrow(coords) != nrow(oulad_clean)) {
    stop("O número de indivíduos em `mca_result` difere de `oulad_clean`.")
  }
  
  dados <- coords %>%
    rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`) %>%
    bind_cols(oulad_clean)
  
  list(
    dados = dados,
    var_dim1 = round(mca_result$eig[1, 2], 1),
    var_dim2 = round(mca_result$eig[2, 2], 1)
  )
}

# Garante independência caso super.R seja carregado sem mca.R
aplicar_eixos_centrais <- function(p) {
  if (exists("eixos_centrais", mode = "function")) {
    return(eixos_centrais(p))
  }
  
  gb <- ggplot_build(p)
  xr <- gb$layout$panel_params[[1]]$x.range
  yr <- gb$layout$panel_params[[1]]$y.range
  seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
  
  eixos <- list(
    annotate("segment", x = xr[1], xend = xr[2], y = 0, yend = 0, color = "black", linewidth = 0.5, arrow = seta),
    annotate("segment", x = 0, xend = 0, y = yr[1], yend = yr[2], color = "black", linewidth = 0.5, arrow = seta)
  )
  
  p$layers <- c(eixos, p$layers)
  p
}

# Calcula limites simétricos/adaptativos com folga para evitar cortes se eixos inverterem
calcular_limites_seguros <- function(df, margem = 0.25) {
  list(
    xlim = range(df$Dim1, na.rm = TRUE) + c(-margem, margem),
    ylim = range(df$Dim2, na.rm = TRUE) + c(-margem, margem)
  )
}

# 2. GRÁFICO 11: FAIXA ETÁRIA (55+) VS. PÓS-GRADUAÇÃO --------------------------
highest_level_and_age_band <- function(mca_result, oulad_clean) {
  mca_info <- extrair_dados_mca(mca_result, oulad_clean)
  
  mca_coords <- mca_info$dados %>%
    mutate(
      eh_55_mais = age_band == "55<=",
      eh_pos_grad = highest_education == "Post Graduate Qualification",
      perfil_destaque = case_when(
        eh_55_mais & eh_pos_grad ~ "Ambos (55+ e Pós-graduação)",
        eh_55_mais               ~ "55+",
        eh_pos_grad              ~ "Pós-graduação",
        TRUE                     ~ "Demais alunos"
      ),
      perfil_destaque = factor(
        perfil_destaque,
        levels = c("55+", "Pós-graduação", "Ambos (55+ e Pós-graduação)", "Demais alunos")
      )
    )
  
  # Centróides calculados sobre os grupos completos (incluindo a interseção)
  centroides <- bind_rows(
    mca_coords %>%
      filter(eh_55_mais) %>%
      summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), grupo = "Centróide: 55+"),
    mca_coords %>%
      filter(eh_pos_grad) %>%
      summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), grupo = "Centróide: Pós-graduação")
  ) %>%
    mutate(grupo = factor(grupo, levels = c("Centróide: 55+", "Centróide: Pós-graduação")))
  
  p_sobreposicao <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_point(
      data = filter(mca_coords, perfil_destaque == "Demais alunos"),
      color = "grey80", alpha = 0.15, size = 1.2
    ) +
    geom_point(
      data = filter(mca_coords, perfil_destaque != "Demais alunos"),
      aes(color = perfil_destaque), alpha = 0.65, size = 1.8
    ) +
    stat_ellipse(
      data = filter(mca_coords, eh_55_mais),
      aes(linetype = "Elipse: 55+"),
      color = "#0072B2", fill = "#0072B2", geom = "polygon",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    stat_ellipse(
      data = filter(mca_coords, eh_pos_grad),
      aes(linetype = "Elipse: Pós-graduação"),
      color = "#CC79A7", fill = "#CC79A7", geom = "polygon",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    geom_point(
      data = centroides,
      aes(shape = grupo),
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
    scale_linetype_manual(
      name = "Fronteiras (68%)",
      values = c("Elipse: 55+" = "solid", "Elipse: Pós-graduação" = "dashed")
    ) +
    scale_shape_manual(
      name = "Centróides",
      values = c("Centróide: 55+" = 16, "Centróide: Pós-graduação" = 17)
    ) +
    guides(
      shape = guide_legend(
        override.aes = list(color = c("#0072B2", "#CC79A7"), size = 4)
      )
    ) +
    labs(
      title = "MCA · Sobreposição entre Faixa Etária (55+) e Pós-Graduação",
      x = paste0("Dim1 (", mca_info$var_dim1, "%)"),
      y = paste0("Dim2 (", mca_info$var_dim2, "%)")
    )
  
  aplicar_eixos_centrais(p_sobreposicao)
}

# 3. GRÁFICO 12: TRAJETÓRIAS DE IMD BAND VS. ESCOLARIDADE ----------------------
imd_band_and_highest_education <- function(mca_result, oulad_clean) {
  mca_info <- extrair_dados_mca(mca_result, oulad_clean)
  
  # Garante ordenação explícita independente do estado prévio dos fatores
  niveis_imd <- c(
    "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
    "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
  )
  niveis_edu <- c(
    "No Formal quals", "Lower Than A Level", "A Level or Equivalent",
    "HE Qualification", "Post Graduate Qualification"
  )
  
  mca_coords <- mca_info$dados %>%
    mutate(
      imd_band = factor(imd_band, levels = niveis_imd),
      highest_education = factor(highest_education, levels = niveis_edu)
    )
  
  cent_imd <- mca_coords %>%
    group_by(categoria = imd_band) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
    arrange(categoria) %>%
    mutate(
      variavel = "IMD Band (Socioeconômico)",
      rotulo = ifelse(
        categoria %in% c("0-10%", "20-30%", "40-50%", "60-70%", "80-90%", "90-100%"),
        as.character(categoria),
        ""
      )
    )
  
  cent_edu <- mca_coords %>%
    group_by(categoria = highest_education) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
    arrange(categoria) %>%
    mutate(
      variavel = "Highest Education (Escolaridade)",
      rotulo = as.character(categoria)
    )
  
  todos_centroides <- bind_rows(cent_imd, cent_edu)
  limites <- calcular_limites_seguros(todos_centroides, margem = 0.65)
  
  p_exp <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_point(color = "grey85", alpha = 0.08, size = 1) +
    geom_path(
      data = cent_edu, aes(group = 1),
      color = "#D55E00", linewidth = 1.4, alpha = 0.45
    ) +
    geom_path(
      data = cent_imd, aes(group = 1),
      color = "#0072B2", linewidth = 0.9, alpha = 0.9
    ) +
    geom_point(
      data = todos_centroides,
      aes(color = variavel, shape = variavel), size = 3.2
    ) +
    geom_text_repel(
      data = filter(cent_imd, rotulo != ""),
      aes(label = rotulo),
      color = "#0072B2", size = 3.1, fontface = "bold",
      nudge_x = -0.35, hjust = 1, direction = "y",
      segment.color = "#0072B2", segment.alpha = 0.5, max.overlaps = Inf
    ) +
    geom_text_repel(
      data = cent_edu,
      aes(label = rotulo),
      color = "#D55E00", size = 3.2, fontface = "bold",
      nudge_x = 0.35, hjust = 0, direction = "y",
      segment.color = "#D55E00", segment.alpha = 0.5, max.overlaps = Inf
    ) +
    scale_color_manual(
      name = "Variável",
      values = c(
        "IMD Band (Socioeconômico)" = "#0072B2",
        "Highest Education (Escolaridade)" = "#D55E00"
      )
    ) +
    scale_shape_manual(
      name = "Variável",
      values = c(
        "IMD Band (Socioeconômico)" = 16,
        "Highest Education (Escolaridade)" = 17
      )
    ) +
    coord_cartesian(xlim = limites$xlim, ylim = limites$ylim) +
    labs(
      title = "MCA · Trajetórias Ordenadas: IMD Band vs. Escolaridade",
      subtitle = "Centróides conectados por ordem crescente (rótulos de IMD à esquerda e de Escolaridade à direita)",
      x = paste0("Dim1 (", mca_info$var_dim1, "%)"),
      y = paste0("Dim2 (", mca_info$var_dim2, "%)")
    )
  
  aplicar_eixos_centrais(p_exp)
}

# 4. GRÁFICO 13: ASSOCIAÇÃO ENTRE MÓDULO E GÊNERO ------------------------------
code_module_and_gender <- function(mca_result, oulad_clean) {
  mca_info <- extrair_dados_mca(mca_result, oulad_clean)
  mca_coords <- mca_info$dados
  
  cent_gender <- mca_coords %>%
    group_by(gender) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop")
  
  cent_modules <- mca_coords %>%
    group_by(code_module) %>%
    summarise(
      Dim1 = mean(Dim1),
      Dim2 = mean(Dim2),
      pct_f = round(mean(gender == "F", na.rm = TRUE) * 100, 0),
      .groups = "drop"
    ) %>%
    mutate(rotulo = paste0(code_module, " (", pct_f, "% F)"))
  
  limites <- calcular_limites_seguros(
    bind_rows(cent_gender, cent_modules),
    margem = 0.85
  )
  
  p_mod_gen <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_point(aes(color = gender), alpha = 0.08, size = 1.2) +
    stat_ellipse(
      aes(color = gender, fill = gender),
      geom = "polygon", alpha = 0.14, level = 0.68, linewidth = 0.9
    ) +
    geom_point(
      data = cent_gender,
      aes(color = gender), size = 3.5, shape = 15, show.legend = FALSE
    ) +
    geom_point(
      data = cent_modules,
      shape = 21, fill = "white", color = "black", stroke = 1.2, size = 3.5
    ) +
    geom_text_repel(
      data = cent_modules,
      aes(label = rotulo),
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
    coord_cartesian(xlim = limites$xlim, ylim = limites$ylim) +
    labs(
      title = "MCA · Associação entre Módulo (code_module) e Gênero",
      subtitle = "Elipses de gênero (68%) sobrepostas aos centróides e proporção feminina de cada módulo",
      x = paste0("Dim1 (", mca_info$var_dim1, "%)"),
      y = paste0("Dim2 (", mca_info$var_dim2, "%)")
    )
  
  aplicar_eixos_centrais(p_mod_gen)
}