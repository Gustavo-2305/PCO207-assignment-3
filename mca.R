# ==============================================================================
# ANÁLISE DE CORRESPONDÊNCIAS MÚLTIPLAS (MCA) - DATASET OULAD
# ==============================================================================

# 1. PACOTES E DEPENDÊNCIAS ----------------------------------------------------
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
tema_bordas <- theme(
  axis.line = element_line(
    color = "black",
    arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
  )
)

# 3. FUNÇÕES AUXILIARES DE GRÁFICOS E EXPORTAÇÃO -------------------------------

# Remove linhas tracejadas padrão (x = 0, y = 0) inseridas pelo factoextra
remover_linhas_tracejadas <- function(p) {
  p$layers <- Filter(
    function(l) !inherits(l$geom, c("GeomHline", "GeomVline")),
    p$layers
  )
  p
}

# Adiciona eixos cartesianos cruzando na origem (0, 0) sem dupla expansão de escala
eixos_centrais <- function(p) {
  p <- remover_linhas_tracejadas(p)
  
  gb <- ggplot_build(p)
  xr <- gb$layout$panel_params[[1]]$x.range
  yr <- gb$layout$panel_params[[1]]$y.range
  
  seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
  
  eixos <- list(
    annotate(
      "segment",
      x = xr[1], xend = xr[2], y = 0, yend = 0,
      color = "black", linewidth = 0.5, arrow = seta
    ),
    annotate(
      "segment",
      x = 0, xend = 0, y = yr[1], yend = yr[2],
      color = "black", linewidth = 0.5, arrow = seta
    )
  )
  
  p$layers <- c(eixos, p$layers)
  
  # Trava os limites já calculados se nenhum coord_cartesian prévio existir
  if (is.null(p$coordinates$limits$x) && is.null(p$coordinates$limits$y)) {
    p <- p + coord_cartesian(xlim = xr, ylim = yr, expand = FALSE)
  }
  
  p
}

dir.create("figuras", showWarnings = FALSE)

salvar_ggplot <- function(plot, filename, width = 8, height = 5.5, dpi = 300) {
  showtext_opts(dpi = dpi)
  on.exit(showtext_opts(dpi = 96), add = TRUE)
  
  ggsave(
    filename = file.path("figuras", filename),
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

# 6. GRÁFICO 1: MATRIZ DE ASSOCIAÇÃO (ETA²) ------------------------------------
eta2_matriz <- rbind(
  mca_result$var$eta2,
  "final_result (sup)" = mca_result$quali.sup$eta2
)

desenhar_corrplot <- function(matriz) {
  op <- par(family = "roboto", col.main = "#1a505c", font.main = 2)
  on.exit(par(op), add = TRUE)
  
  col_mca <- colorRampPalette(c("white", "#6baed6", "#2171b5"))
  corrplot(
    matriz,
    is.corr = FALSE,
    col.lim = c(0, max(matriz)),
    col = col_mca(20),
    addCoef.col = "black",
    number.cex = 0.72,
    tl.col = "black",
    cl.cex = 0.8,
    tl.cex = 0.9,
    mar = c(0, 0, 2.5, 0),
    cl.offset = 0.2,
    title = "MCA · Associação das Variáveis (Eta²)"
  )
}

salvar_corrplot <- function(matriz, filename, width = 7, height = 6, dpi = 300) {
  showtext_opts(dpi = dpi)
  on.exit(showtext_opts(dpi = 96), add = TRUE)
  
  png(
    file.path("figuras", filename),
    width = width, height = height, units = "in", res = dpi, bg = "white"
  )
  on.exit(dev.off(), add = TRUE)
  
  desenhar_corrplot(matriz)
}

desenhar_corrplot(eta2_matriz)
salvar_corrplot(eta2_matriz, "01_associacao_eta2.png")

# 7. GRÁFICO 2: SCREE PLOT -----------------------------------------------------
p_scree <- fviz_eig(
  mca_result,
  addlabels = TRUE,
  title = "MCA · Scree Plot",
  ggtheme = theme_oulad
) +
  tema_bordas

print(p_scree)
salvar_ggplot(p_scree, "02_scree_plot.png", width = 7.5, height = 5)

# 8. GRÁFICO 3: CORRELAÇÃO DAS VARIÁVEIS COM OS EIXOS --------------------------
p_cor <- fviz_mca_var(
  mca_result,
  choice = "mca.cor",
  repel = TRUE,
  title = "MCA · Correlação das Variáveis com os Eixos",
  ggtheme = theme_oulad
) %>%
  remover_linhas_tracejadas() +
  tema_bordas

print(p_cor)
salvar_ggplot(p_cor, "03_correlacao_variaveis.png", width = 8, height = 6)

# 9. GRÁFICOS 4 A 10: ELIPSES DE CONCENTRAÇÃO POR VARIÁVEL ---------------------
vars_grupo <- names(oulad_mca)

for (i in seq_along(vars_grupo)) {
  v <- vars_grupo[i]
  p_grupo <- fviz_mca_ind(
    mca_result,
    axes = c(1, 2),
    geom.ind = "point",
    alpha.ind = 0.05,
    habillage = oulad_mca[[v]],
    addEllipses = TRUE,
    ellipse.level = 0.68,
    label = "none",
    title = paste("MCA · Grupos por", v),
    ggtheme = theme_oulad
  ) %>%
    eixos_centrais()
  
  print(p_grupo)
  salvar_ggplot(p_grupo, sprintf("%02d_mca_grupo_%s.png", i + 3, v), width = 8, height = 6)
}

# 10. GRÁFICOS 11 A 13: SOBREPOSIÇÕES PERSONALIZADAS (super.R) -----------------
p_sup1 <- highest_level_and_age_band(mca_result, oulad_clean)
print(p_sup1)
salvar_ggplot(p_sup1, "11_idade_pos_graduacao.png", width = 8.5, height = 6)

p_sup2 <- imd_band_and_highest_education(mca_result, oulad_clean)
print(p_sup2)
salvar_ggplot(p_sup2, "12_imd_escolaridade.png", width = 8.5, height = 6)

p_sup3 <- code_module_and_gender(mca_result, oulad_clean)
print(p_sup3)
salvar_ggplot(p_sup3, "13_modulo_genero.png", width = 8.5, height = 6)