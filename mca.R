# ==============================================================================
# ANÁLISE DE CORRESPONDÊNCIAS MÚLTIPLAS (MCA) - DATASET OULAD
# ==============================================================================

# 1. DEPENDÊNCIAS --------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(showtext)

options(ggrepel.max.overlaps = Inf)

source("prep_data.R")
source("super.R")

# 2. CONFIGURAÇÃO DE TIPOGRAFIA E TEMA VISUAL ----------------------------------
font_add_google("Roboto", "roboto")
showtext_opts(dpi = 96)
showtext_auto()

theme_oulad <- theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(color = "#1a505c", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "grey30", hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_blank(),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

theme_set(theme_oulad)
update_geom_defaults("text", list(family = "roboto"))
update_geom_defaults("text_repel", list(family = "roboto"))

# Setas nas bordas inferior e esquerda (gráficos restritos ao 1º quadrante)
theme_border_axes <- theme(
  axis.line = element_line(
    color = "black",
    arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
  )
)

# 3. DICIONÁRIOS DE RÓTULOS E FUNÇÕES AUXILIARES -------------------------------
output_dir <- "figures"
dir.create(output_dir, showWarnings = FALSE)

var_labels <- c(
  gender = "Gênero",
  age_band = "Faixa Etária",
  highest_education = "Escolaridade",
  imd_band = "Faixa IMD",
  region = "Região",
  code_module = "Módulo",
  final_result = "Resultado Final"
)

category_labels <- list(
  gender = c(
    "M" = "Masculino",
    "F" = "Feminino"
  ),
  age_band = c(
    "0-35" = "0 a 35 anos",
    "35-55" = "35 a 55 anos",
    "55<=" = "55 anos ou mais"
  ),
  highest_education = c(
    "No Formal quals" = "Sem qualificação formal",
    "Lower Than A Level" = "Inferior ao Ensino Médio",
    "A Level or Equivalent" = "Ensino Médio ou equivalente",
    "HE Qualification" = "Ensino Superior",
    "Post Graduate Qualification" = "Pós-graduação"
  ),
  region = c(
    "East Anglian Region" = "East Anglia",
    "Scotland" = "Escócia",
    "North Western Region" = "Noroeste da Inglaterra",
    "South East Region" = "Sudeste da Inglaterra",
    "West Midlands Region" = "West Midlands",
    "Wales" = "País de Gales",
    "North Region" = "Norte da Inglaterra",
    "South Region" = "Sul da Inglaterra",
    "Ireland" = "Irlanda",
    "South West Region" = "Sudoeste da Inglaterra",
    "East Midlands Region" = "East Midlands",
    "Yorkshire Region" = "Yorkshire",
    "London Region" = "Londres"
  ),
  final_result = c(
    "Pass" = "Aprovado",
    "Fail" = "Reprovado",
    "Withdrawn" = "Desistente",
    "Distinction" = "Distinção"
  )
)

translate_labels <- function(values, label_map) {
  if (is.null(label_map)) {
    return(values)
  }
  
  if (is.factor(values)) {
    orig_levels <- levels(values)
    new_levels <- ifelse(
      orig_levels %in% names(label_map),
      unname(label_map[orig_levels]),
      orig_levels
    )
    levels(values) <- new_levels
    return(values)
  }
  
  translated <- ifelse(
    values %in% names(label_map),
    unname(label_map[values]),
    values
  )
  factor(translated)
}

format_dim_label <- function(mca_res, dim_index) {
  pct <- formatC(
    mca_res$eig[dim_index, 2],
    format = "f",
    digits = 1,
    decimal.mark = ","
  )
  sprintf("Dimensão %d (%s%%)", dim_index, pct)
}

# Remove linhas tracejadas padrão (x = 0, y = 0) inseridas pelo factoextra
remove_dashed_lines <- function(plot) {
  plot$layers <- Filter(
    function(layer) !inherits(layer$geom, c("GeomHline", "GeomVline")),
    plot$layers
  )
  plot
}

# Adiciona eixos cartesianos cruzando na origem
add_center_axes <- function(plot) {
  plot <- remove_dashed_lines(plot)
  
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
  
  # Trava os limites já calculados se nenhum coord_cartesian prévio existir
  if (is.null(plot$coordinates$limits$x) && is.null(plot$coordinates$limits$y)) {
    plot <- plot + coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE)
  }
  
  plot
}

save_ggplot <- function(plot, filename, width = 8, height = 5.5, dpi = 300) {
  showtext_opts(dpi = dpi)
  on.exit(showtext_opts(dpi = 96), add = TRUE)
  
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
}

# 4. CARREGAMENTO E PREPARAÇÃO DOS DADOS ---------------------------------------
oulad <- prep_data()

oulad_clean <- oulad %>%
  # Mantém a mesma amostra válida utilizada nas análises quantitativas/mistas
  filter(!is.na(total_clicks), !is.na(avg_grade)) %>%
  mutate(
    imd_band = fct_relevel(
      imd_band,
      "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
      "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
    ),
    highest_education = fct_relevel(
      highest_education,
      "No Formal quals", "Lower Than A Level", "A Level or Equivalent",
      "HE Qualification", "Post Graduate Qualification"
    )
  ) %>%
  drop_na(
    gender, age_band, highest_education, imd_band,
    region, code_module, final_result
  )

# 5. EXECUÇÃO DA MCA -----------------------------------------------------------
oulad_mca <- oulad_clean %>%
  select(
    # Colunas 1 a 6: Variáveis categóricas ativas
    gender, age_band, highest_education, imd_band, region, code_module,
    # Coluna 7: Variável categórica suplementar (desfecho acadêmico)
    final_result
  )

mca_result <- MCA(
  oulad_mca,
  ncp = 5,
  quali.sup = 7,
  graph = FALSE
)

# 6. GRÁFICO 1: MATRIZ DE ASSOCIAÇÃO -------------------------------------------
eta2_matrix <- rbind(
  mca_result$var$eta2,
  mca_result$quali.sup$eta2
)

row_keys <- rownames(eta2_matrix)
rownames(eta2_matrix) <- ifelse(
  row_keys == "final_result",
  unname(var_labels["final_result"]),
  unname(var_labels[row_keys])
)
colnames(eta2_matrix) <- paste("Dimensão", seq_len(ncol(eta2_matrix)))

make_circle_df <- function(df, max_value, max_radius = 0.44, n_points = 60) {
  angles <- seq(0, 2 * pi, length.out = n_points)
  
  df %>%
    mutate(
      x_center = as.numeric(dimension),
      y_center = as.numeric(variable),
      radius = max_radius * sqrt(pmax(eta2, 0) / max_value),
      cell_id = row_number()
    ) %>%
    reframe(
      x = x_center + radius * cos(angles),
      y = y_center + radius * sin(angles),
      eta2 = eta2,
      .by = cell_id
    )
}

eta2_df <- as.data.frame(eta2_matrix) %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(
    cols = -variable,
    names_to = "dimension",
    values_to = "eta2"
  ) %>%
  mutate(
    variable = factor(variable, levels = rev(rownames(eta2_matrix))),
    dimension = factor(dimension, levels = colnames(eta2_matrix))
  )

upper_limit <- ceiling(max(eta2_df$eta2) * 10) / 10
circles_df <- make_circle_df(eta2_df, max_value = upper_limit)

plot_eta2 <- ggplot() +
  geom_tile(
    data = eta2_df,
    aes(x = as.numeric(dimension), y = as.numeric(variable)),
    fill = "white",
    color = "black",
    linewidth = 0.4
  ) +
  geom_polygon(
    data = circles_df,
    aes(x = x, y = y, group = cell_id, fill = eta2),
    color = NA
  ) +
  scale_x_continuous(
    breaks = seq_len(ncol(eta2_matrix)),
    labels = colnames(eta2_matrix),
    position = "top",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq_len(nrow(eta2_matrix)),
    labels = rev(rownames(eta2_matrix)),
    expand = c(0, 0)
  ) +
  scale_fill_gradientn(
    colors = c("indianred", "white", "steelblue"),
    limits = c(0, upper_limit),
    breaks = seq(0, upper_limit, length.out = 5),
    labels = function(x) formatC(x, format = "f", digits = 2, decimal.mark = ","),
    name = NULL
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.55, "cm"),
      barheight = unit(9, "cm"),
      theme = theme(
        legend.frame = element_rect(color = "black", linewidth = 0.4),
        legend.ticks = element_line(color = "black", linewidth = 0.4)
      )
    )
  ) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(
    title = "MCA · Associação das Variáveis",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b = 8)),
    plot.margin = margin(t = 10, r = 10, b = 8, l = 10)
  )

print(plot_eta2)
save_ggplot(
  plot_eta2,
  "01_eta2_association.png",
  width = 5.8,
  height = 5.4
)

# 7. GRÁFICO 2: VARIÂNCIA EXPLICADA --------------------------------------------
plot_scree <- fviz_eig(
  mca_result,
  addlabels = TRUE,
  ggtheme = theme_oulad,
  ylim = c(0, 10)
) +
  labs(
    title = "MCA · Variância Explicada por Dimensão",
    x = "Dimensões",
    y = "Variância explicada (%)"
  ) +
  theme_border_axes

print(plot_scree)
save_ggplot(
  plot_scree,
  "02_scree_plot.png",
  width = 7.5,
  height = 5
)

# 8. GRÁFICO 3: CORRELAÇÃO DAS VARIÁVEIS COM OS EIXOS --------------------------
mca_cor_display <- mca_result
rownames(mca_cor_display$var$eta2) <- unname(
  var_labels[rownames(mca_cor_display$var$eta2)]
)
rownames(mca_cor_display$quali.sup$eta2) <- unname(
  var_labels[rownames(mca_cor_display$quali.sup$eta2)]
)

plot_cor <- fviz_mca_var(
  mca_cor_display,
  choice = "mca.cor",
  repel = TRUE,
  ggtheme = theme_oulad
) %>%
  remove_dashed_lines() +
  labs(
    title = "MCA · Correlação das Variáveis com os Eixos",
    x = format_dim_label(mca_result, 1),
    y = format_dim_label(mca_result, 2)
  ) +
  theme_border_axes

print(plot_cor)
save_ggplot(
  plot_cor,
  "03_variable_correlation.png",
  width = 8,
  height = 6
)

# 9. GRÁFICOS 4 A 10: ELIPSES DE CONCENTRAÇÃO POR VARIÁVEL ---------------------
group_vars <- names(oulad_mca)

for (i in seq_along(group_vars)) {
  var_name <- group_vars[i]
  var_label <- unname(var_labels[var_name])
  group_values <- translate_labels(
    oulad_mca[[var_name]],
    category_labels[[var_name]]
  )
  
  plot_group <- fviz_mca_ind(
    mca_result,
    axes = c(1, 2),
    geom.ind = "point",
    alpha.ind = 0.05,
    habillage = group_values,
    addEllipses = TRUE,
    ellipse.level = 0.68,
    label = "none",
    legend.title = var_label,
    ggtheme = theme_oulad
  ) +
    labs(
      title = paste("ACM · Grupos por", var_label),
      x = format_dim_label(mca_result, 1),
      y = format_dim_label(mca_result, 2)
    )
  
  plot_group <- add_center_axes(plot_group)
  
  print(plot_group)
  save_ggplot(
    plot_group,
    sprintf("%02d_mca_group_%s.png", i + 3, var_name),
    width = 8,
    height = 6
  )
}

# 10. GRÁFICOS 11 A 13: SOBREPOSIÇÕES ------------------------------------------
plot_overlay_1 <- highest_level_and_age_band(mca_result, oulad_clean)
print(plot_overlay_1)
save_ggplot(
  plot_overlay_1,
  "11_age_postgraduate.png",
  width = 8.5,
  height = 6
)

plot_overlay_2 <- imd_band_and_highest_education(mca_result, oulad_clean)
print(plot_overlay_2)
save_ggplot(
  plot_overlay_2,
  "12_imd_education.png",
  width = 8.5,
  height = 6
)

plot_overlay_3 <- code_module_and_gender(mca_result, oulad_clean)
print(plot_overlay_3)
save_ggplot(
  plot_overlay_3,
  "13_module_gender.png",
  width = 8.5,
  height = 6
)