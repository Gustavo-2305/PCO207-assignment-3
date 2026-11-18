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
oulad_pca <- oulad %>% select(c(6, 11, 12)) %>%
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