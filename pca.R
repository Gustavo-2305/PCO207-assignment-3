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
oulad_pca <- oulad %>% select(c(6, 9, 11, 12)) %>%
  mutate(
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
# col = col2(20): generates a 20 color pallete from the previously defined col2 gradient
# mar: defines the graph margins
# cl.offset: defines the legend offset
plot1 = corrplot(pca_result$var$coord, is.coord = FALSE, col = col2(20), mar = c(0,0,2,0), cl.offset = 0.2, title = "Title...")
print(plot1)