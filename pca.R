library(tidyverse)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(fastDummies)
library(showtext)
library(sysfonts)

# creates the custom theme
font_add_google("Roboto", "roboto")
showtext_auto()

theme_oulad <- theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      color = "#1a505c",
      face = "bold",
      hjust = 0.5
    ),
    
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.line = element_line(
      color = "black",
      arrow = grid::arrow(
        length = grid::unit(0.2, "cm"),
        type = "closed"
      )
    ),
    
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

# creats the tabbles to calculate the PCA
if (1) {
  source("prep_data.R")
  oulad <- prep_data()
  glimpse(oulad)

oulad <- oulad %>% 
  filter(!is.na(total_clicks), !is.na(wei_grade)) %>%
  mutate(
    final_result = factor(
      tolower(as.character(final_result)),
      levels = c("withdrawn", "fail", "pass", "distinction"),
      labels = c("Withdrawn", "Fail", "Pass", "Distinction")
    )
  )

oulad_pca <- oulad %>%
  select(total_clicks, wei_grade, num_assessments, avg_late, num_of_prev_attempts) %>%
  mutate(
    total_clicks = log1p(total_clicks), # converts total_clicks to a log scale
    f_attempt = case_when(
      num_of_prev_attempts == 0 ~ 1,
      num_of_prev_attempts > 0 ~ 2,
      TRUE ~ NA_real_
    ) 
  ) %>%
  select(-num_of_prev_attempts)
  
glimpse(oulad_pca)
}

# scale.unit = TRUE: centers and divides each variable by their standard deviation
# ncp = ncol(oulad_pca): number of columns of oulad_pca
# graph = FALSE: disables default graphs
pca_result = oulad_pca %>% PCA(scale.unit = TRUE, ncp = ncol(oulad_pca), graph = FALSE)

# creates a gradient from darkred to darkblue
col2 = colorRampPalette(c('indianred3', 'white', 'steelblue'))

# pca_result$var$coord: variables on the rows; components on the columns
# is.corr = FALSE: treats the entry as a generic matrix
# col = col2(20): generates a 20 color palette from the previously defined col2 gradient
# mar: defines the graph margins
# cl.offset: defines the legend offset
# save current graphical parameters
old_par = par(no.readonly = TRUE)

# apply OULAD visual style
par(
  family = "roboto",
  col.main = "#1a505c",
  font.main = 2,
  cex.main = 1.1,
  col.axis = "black",
  fg = "black"
)

# create Bubble Plot
plot1 = corrplot(
  pca_result$var$coord,
  is.corr = FALSE,
  col = col2(20),
  mar = c(0, 0, 2, 0),
  cl.offset = 0.2,
  cl.length = 5,
  cl.align.text = "l",
  
  # text customization
  tl.col = "black",
  tl.cex = 0.9,
  
  # color legend customization
  cl.cex = 0.8,
  
  # grid customization
  addgrid.col = "black",
  
  title = "PCA · Associação das Variáveis"
)

# restore previous graphical parameters
par(old_par)
# print(plot1)

# fviz_eig(): percentage of variance explained be each component
# addlabels = TRUE: writes percentages on the plot
# ylim = c(0, 100): sets y-axis limits from 0 to 100
plot2 = fviz_eig(
  pca_result, 
  addlabels = TRUE, 
  ylim = c(0, 100),
  title = "PCA · Scree Plot"
  )
# print(plot2)

# col.var = "black": sets the colors of arrows and labels
# axes = c(1,2): PC1 on x-aixs; PC2 on y-axis
# select.var = list(contrib = 5): selects up to the five highest-contributing variables for each axis 
plot3 = fviz_pca_var(
  pca_result, 
  col.var = 'black',
  axes = c(1,2), 
  select.var = list(contrib = 5),
  repel = TRUE,
  labelsize = 4,
  title = "PCA · Círculo de Correlação"
  )
# print(plot3)

plot4 = fviz_pca_ind(
  pca_result, 
  axes = c(1, 2), 
  geom = "point",
  col.ind = "steelblue",
  alpha.ind = 0.1,
  geom.ind = "point",
  title = "PCA · Primeiro"
  )
# print(plot4)

plot5 = fviz_pca_biplot(
  pca_result,
  axes = c(1, 2),
  col.var = "black",
  geom.ind = "point",
  col.ind = oulad$num_of_prev_attempts,
  gradient.cols = c("steelblue", "indianred3"),
  alpha.ind = 0.1,
  title = "PCA · Biplot (First Attempt)"
  )
# print(plot5)

plot6 = fviz_pca_biplot(
  pca_result,
  axes = c(1, 2),
  col.var = "black",
  geom.ind = "point",
  habillage = oulad$final_result,
  palette = grDevices::colorRampPalette(c("indianred3", "steelblue"))(4),
  shape.ind = factor(rep("point", length(oulad$final_result))),
  alpha.ind = 0.1,
  title = "PCA · Biplot (Final Result)"
) +
  guides(shape = "none")
# print(plot6)

for (i in 2:6) {
  nome_plot = paste0("plot", i)
  
  assign(
    nome_plot,
    get(nome_plot) +
      theme_oulad
  )
}

# applies the custom theme to the plots
for (i in 4:6) {
  nome_plot = paste0("plot", i)
  
  assign(
    nome_plot,
    get(nome_plot) +
      coord_cartesian(
        xlim = c(-6, 6),
        ylim = c(-6, 6),
        expand = FALSE
      ) +
      scale_x_continuous(breaks = seq(-6, 6, 2)) +
      scale_y_continuous(breaks = seq(-6, 6, 2))
  )
}

for (i in 3:3) {
  nome_plot = paste0("plot", i)
  
  p = get(nome_plot)
  
  # Remove the original dashed reference lines
  p$layers = Filter(
    function(layer) {
      !inherits(layer$geom, "GeomHline") &&
        !inherits(layer$geom, "GeomVline")
    },
    p$layers
  )
  
  p = p +
    
    # Removes the original axes from theme_oulad
    theme(
      axis.line = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    ) +
    
    # Central horizontal axis (X)
    annotate(
      "segment",
      x = -1.25, xend = 1.25,
      y = 0, yend = 0,
      colour = "black",
      linewidth = 0.5,
      arrow = grid::arrow(
        length = grid::unit(0.2, "cm"),
        type = "closed"
      )
    ) +
    
    # Central vertical axis (Y)
    annotate(
      "segment",
      x = 0, xend = 0,
      y = -1.25, yend = 1.25,
      colour = "black",
      linewidth = 0.5,
      arrow = grid::arrow(
        length = grid::unit(0.2, "cm"),
        type = "closed"
      )
    )
  
  assign(nome_plot, p)
}

for (i in 4:6) {
  nome_plot = paste0("plot", i)
  
  p = get(nome_plot)
  
  # Remove the original dashed reference lines
  p$layers = Filter(
    function(layer) {
      !inherits(layer$geom, "GeomHline") &&
        !inherits(layer$geom, "GeomVline")
    },
    p$layers
  )
  
  p = p +
    
    # Removes the original axes from theme_oulad
    theme(
      axis.line = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    ) +
    
    # Central horizontal axis (X)
    annotate(
      "segment",
      x = -6, xend = 6,
      y = 0, yend = 0,
      colour = "black",
      linewidth = 0.5,
      arrow = grid::arrow(
        length = grid::unit(0.2, "cm"),
        type = "closed"
      )
    ) +
    
    # Central vertical axis (Y)
    annotate(
      "segment",
      x = 0, xend = 0,
      y = -6, yend = 6,
      colour = "black",
      linewidth = 0.5,
      arrow = grid::arrow(
        length = grid::unit(0.2, "cm"),
        type = "closed"
      )
    )
  
  assign(nome_plot, p)
}

for (i in 1:6) {
  print(get(paste0("plot", i)))
}