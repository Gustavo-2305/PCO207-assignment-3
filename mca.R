library(tidyverse)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(showtext)
library(sysfonts)

options(ggrepel.max.overlaps = Inf)

source("prep_data.R")
oulad <- prep_data()

#####
# FONTE, TEMA PERSONALIZADO E FUNÇÃO DE EXPORTAÇÃO
#####
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
    axis.line = element_line(
      color = "black",
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")
    ),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

theme_set(theme_oulad)
update_geom_defaults("text", list(family = "roboto"))
update_geom_defaults("text_repel", list(family = "roboto"))

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

#####
# PREPARAÇÃO DOS DADOS
#####
oulad_clean <- oulad %>%
  filter(!is.na(total_clicks), !is.na(avg_grade)) %>%
  mutate(
    total_clicks_log = log1p(total_clicks),
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
  drop_na(imd_band, highest_education, gender, age_band, region, code_module, final_result)

#####
# MCA (VARIÁVEIS CATEGÓRICAS)
#####
oulad_mca <- oulad_clean %>%
  select(
    # Colunas 1 a 6: Categóricas ativas
    gender, age_band, highest_education, imd_band, region, code_module,
    # Coluna 7: Suplementar
    final_result
  )

mca_result <- MCA(
  oulad_mca,
  ncp = 5,
  quali.sup = 7,
  graph = FALSE
)

# Gráfico 1: Associação (Eta²)
desenhar_corrplot <- function() {
  op <- par(family = "roboto", col.main = "#1a505c", font.main = 2)
  on.exit(par(op), add = TRUE)
  
  col_mca <- colorRampPalette(c("white", "darkblue"))
  corrplot(
    mca_result$var$eta2,
    is.corr = FALSE,
    col = col_mca(20),
    tl.col = "black",
    cl.cex = 0.8,
    tl.cex = 0.9,
    mar = c(0, 0, 2, 0),
    cl.offset = 0.2,
    title = "MCA · Associação (Eta²)"
  )
}

desenhar_corrplot()

showtext_opts(dpi = 300)
png("figuras/01_associacao_eta2.png", width = 7, height = 6, units = "in", res = 300, bg = "white")
desenhar_corrplot()
dev.off()
showtext_opts(dpi = 96)

# Gráfico 2: Scree Plot
p_scree <- fviz_eig(
  mca_result,
  addlabels = TRUE,
  title = "MCA · Scree Plot",
  ggtheme = theme_oulad
)
print(p_scree)
salvar_ggplot(p_scree, "02_scree_plot.png", width = 7.5, height = 5)

# Gráfico 3: Correlação geral das variáveis com os eixos
p_cor <- fviz_mca_var(
  mca_result,
  choice = "mca.cor",
  repel = TRUE,
  title = "MCA · Correlação das Variáveis",
  ggtheme = theme_oulad
)
print(p_cor)
salvar_ggplot(p_cor, "03_correlacao_variaveis.png", width = 8, height = 6)

# Gráficos 4 a 10: Elipses por grupo
vars_grupo <- names(oulad_mca)

for (i in seq_along(vars_grupo)) {
  v <- vars_grupo[i]
  p <- fviz_mca_ind(
    mca_result,
    axes = c(1, 2),
    geom.ind = "point",
    alpha.ind = 0.05,
    habillage = oulad_clean[[v]],
    addEllipses = TRUE,
    ellipse.level = 0.68,
    label = "none",
    title = paste("MCA Grupos -", v),
    ggtheme = theme_oulad
  )
  print(p)
  salvar_ggplot(p, sprintf("%02d_mca_grupo_%s.png", i + 3, v), width = 8, height = 6)
}

# Gráficos 11 a 13: Gráficos combinados personalizados
source("super.R")

p_sup1 <- highest_level_and_age_band(mca_result, oulad_clean)
salvar_ggplot(p_sup1, "11_idade_pos_graduacao.png", width = 8.5, height = 6)

p_sup2 <- imd_band_and_highest_education(mca_result, oulad_clean)
salvar_ggplot(p_sup2, "12_imd_escolaridade.png", width = 8.5, height = 6)

p_sup3 <- code_module_and_gender(mca_result, oulad_clean)
salvar_ggplot(p_sup3, "13_modulo_genero.png", width = 8.5, height = 6)