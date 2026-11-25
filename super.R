library(tidyverse)
library(ggrepel)

highest_level_and_age_band <- function(mca_result, oulad_clean) {
  # 1. Extrai as coordenadas dos indivíduos da MCA e cruza com a base limpa
  mca_coords <- as.data.frame(mca_result$ind$coord) %>%
    rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`) %>%
    bind_cols(oulad_clean) %>%
    mutate(
      perfil_destaque = case_when(
        age_band == "55<=" ~ "55+",
        highest_education == "Post Graduate Qualification" ~ "Pós-graduação",
        TRUE ~ "Demais alunos"
      ),
      perfil_destaque = factor(perfil_destaque, levels = c(
        "55+",
        "Pós-graduação",
        "Demais alunos"
      ))
    )
  
  # 2. Calcula os centróides exatos de cada grupo completo
  centroides <- bind_rows(
    mca_coords %>%
      filter(age_band == "55<=") %>%
      summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), grupo = "Centróide: 55+"),
    mca_coords %>%
      filter(highest_education == "Post Graduate Qualification") %>%
      summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), grupo = "Centróide: Pós-graduação")
  )
  
  # Extrai os percentuais de variância explicada para os eixos
  var_dim1 <- round(mca_result$eig[1, 2], 1)
  var_dim2 <- round(mca_result$eig[2, 2], 1)
  
  # 3. Constrói o gráfico combinado
  p_sobreposicao <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(
      data = filter(mca_coords, perfil_destaque == "Demais alunos"),
      color = "grey80", alpha = 0.15, size = 1.2
    ) +
    geom_point(
      data = filter(mca_coords, perfil_destaque != "Demais alunos"),
      aes(color = perfil_destaque), alpha = 0.65, size = 1.8
    ) +
    stat_ellipse(
      data = filter(mca_coords, age_band == "55<="),
      aes(linetype = "Elipse: 55+"),
      color = "#0072B2", fill = "#0072B2", geom = "polygon",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    stat_ellipse(
      data = filter(mca_coords, highest_education == "Post Graduate Qualification"),
      aes(linetype = "Elipse: Pós-graduação"),
      color = "#CC79A7", fill = "#CC79A7", geom = "polygon",
      alpha = 0.12, level = 0.68, linewidth = 0.9
    ) +
    geom_point(
      data = centroides,
      aes(shape = grupo), color = c("#0072B2", "#CC79A7"), size = 4
    ) +
    scale_color_manual(
      name = "Indivíduos Destacados",
      values = c("55+" = "#0072B2", "Pós-graduação" = "#CC79A7")
    ) +
    scale_linetype_manual(
      name = "Fronteiras",
      values = c("Elipse: 55+" = "solid", "Elipse: Pós-graduação" = "dashed")
    ) +
    scale_shape_manual(
      name = "Centróides",
      values = c("Centróide: 55+" = 16, "Centróide: Pós-graduação" = 17)
    ) +
    labs(
      title = "Sobreposição entre Faixa Etária (55+) e Pós-Graduação",
      x = paste0("Dim1 (", var_dim1, "%)"),
      y = paste0("Dim2 (", var_dim2, "%)")
    )
  
  print(p_sobreposicao)
  invisible(p_sobreposicao)
}

imd_band_and_highest_education <- function(mca_result, oulad_clean) {
  # 1. Extrai coordenadas dos indivíduos da MCA
  mca_coords <- as.data.frame(mca_result$ind$coord) %>%
    rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`) %>%
    bind_cols(oulad_clean)
  
  # 2. Centróides ordenados de IMD Band (10 níveis)
  cent_imd <- mca_coords %>%
    group_by(categoria = imd_band) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
    mutate(
      variavel = "IMD Band (Socioeconômico)",
      rotulo = ifelse(
        categoria %in% c("0-10%", "20-30%", "40-50%", "60-70%", "80-90%", "90-100%"),
        as.character(categoria),
        ""
      )
    )
  
  # 3. Centróides ordenados de Escolaridade (5 níveis)
  cent_edu <- mca_coords %>%
    group_by(categoria = highest_education) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
    mutate(
      variavel = "Highest Education (Escolaridade)",
      rotulo = as.character(categoria)
    )
  
  var_dim1 <- round(mca_result$eig[1, 2], 1)
  var_dim2 <- round(mca_result$eig[2, 2], 1)
  
  # 4. Constrói o gráfico limpo com zoom e separação lateral de textos
  p_exp <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
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
      data = bind_rows(cent_imd, cent_edu),
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
    coord_cartesian(xlim = c(-1.1, 1.8), ylim = c(-0.8, 1.5)) +
    labs(
      title = "Sobreposição Exploratória: IMD Band vs. Escolaridade",
      subtitle = "Trajetórias ordenadas dos centróides (IMD Band à esquerda; Escolaridade à direita)",
      x = paste0("Dim1 (", var_dim1, "%)"),
      y = paste0("Dim2 (", var_dim2, "%)")
    )
  
  print(p_exp)
  invisible(p_exp)
}

code_module_and_gender <- function(mca_result, oulad_clean) {
  # 1. Extrai coordenadas da MCA e une com a base limpa
  mca_coords <- as.data.frame(mca_result$ind$coord) %>%
    rename(Dim1 = `Dim 1`, Dim2 = `Dim 2`) %>%
    bind_cols(oulad_clean)
  
  # 2. Calcula centróides dos gêneros
  cent_gender <- mca_coords %>%
    group_by(gender) %>%
    summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop")
  
  # 3. Calcula centróides dos 7 módulos e o percentual feminino de cada um
  cent_modules <- mca_coords %>%
    group_by(code_module) %>%
    summarise(
      Dim1 = mean(Dim1),
      Dim2 = mean(Dim2),
      pct_f = round(mean(gender == "F") * 100, 0),
      .groups = "drop"
    ) %>%
    mutate(rotulo = paste0(code_module, " (", pct_f, "% F)"))
  
  var_dim1 <- round(mca_result$eig[1, 2], 1)
  var_dim2 <- round(mca_result$eig[2, 2], 1)
  
  # 4. Constrói o gráfico combinando elipses de gênero e centróides dos módulos
  p_mod_gen <- ggplot(mca_coords, aes(x = Dim1, y = Dim2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
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
    coord_cartesian(xlim = c(-1.3, 1.6), ylim = c(-1.2, 1.6)) +
    labs(
      title = "MCA - Associação entre Módulo (code_module) e Gênero",
      subtitle = "Elipses de gênero (68%) sobrepostas aos centróides e proporção feminina de cada módulo",
      x = paste0("Dim1 (", var_dim1, "%)"),
      y = paste0("Dim2 (", var_dim2, "%)")
    )
  
  print(p_mod_gen)
  invisible(p_mod_gen)
}