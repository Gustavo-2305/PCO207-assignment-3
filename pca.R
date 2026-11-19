library(tidyverse)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(fastDummies)

# source("prep_data.R")
# oulad <- prep_data()
# glimpse(oulad)

# creates a table for the PCA
# selects only active columns and converts factors to ranked numbers
oulad_pca <- oulad %>% select(c(1, 6, 9, 11, 12)) %>%
  mutate(
    code_module = code_module %>%
      fct_relevel("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG") %>%
      as.numeric(),
    highest_education = highest_education %>%
      fct_relevel("No Formal quals", "Lower Than A Level", "A Level or Equivalent", "HE Qualification", "Post Graduate Qualification") %>%
      as.numeric()
  )
glimpse(oulad_pca)

# scale.unit = TRUE: centers and divides each variable by their standard deviation
# ncp = ncol(oulad_pca): number of columns of oulad_pca
# graph = FALSE: disables default graphs
pca_result = oulad_pca %>% PCA(scale.unit = TRUE, ncp = ncol(oulad_pca), graph = FALSE)

# creates a gradient from darkred to darkblue
col2 = colorRampPalette(c('darkred', 'white', 'darkblue'))

# pca_result$var$coord: variables on the rows; components on the columns
# is.corr = FALSE: treats the entry as a generic matrix
# col = col2(20): generates a 20 color palette from the previously defined col2 gradient
# mar: defines the graph margins
# cl.offset: defines the legend offset
plot1 = corrplot(pca_result$var$coord, is.coord = FALSE, col = col2(20), mar = c(0,0,2,0), cl.offset = 0.2, title = "Title...")
print(plot1)

# fviz_eig(): percentage of variance explained be each component
# addlabels = TRUE: writes percentages on the plot
# ylim = c(0, 100): sets y-axis limits from 0 to 100
plot2 = fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))
print(plot2)

# col.var = "black": sets the colors of arrows and labels
# axes = c(1,2): PC1 on x-aixs; PC2 on y-axis
# select.var = list(contrib = 5): selects up to the five highest-contributing variables for each axis 
plot3 = fviz_pca_var(pca_result, col.var = 'black', axes = c(1,2), select.var = list(contrib = 5), title = "First PCA of var")
print(plot3)

# oulad$final_result: uses the final result to color the points
# geom.ind = "point": hides the labels
plot4 = fviz_pca_ind(pca_result, col.ind = oulad$final_result, axes = c(1, 2), geom.ind = "point", title = "First PCA of ind")
print(plot4)

plot5 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black", col.ind = oulad$final_result, geom.ind = "point", title = "First PCA biplot")
print(plot5)