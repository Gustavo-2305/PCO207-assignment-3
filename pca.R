library(tidyverse)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(fastDummies)

if (0) {
  source("prep_data.R")
  oulad <- prep_data()
  glimpse(oulad)

oulad <- oulad %>% 
  filter(!is.na(total_clicks), !is.na(avg_grade)) %>%
  mutate(
    imd_band = imd_band %>%
      fct_relevel("0-10%", "10-20", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%") %>%
      as.numeric(),
    highest_education = highest_education %>%
      fct_relevel("No Formal quals", "Lower Than A Level", "A Level or Equivalent", "HE Qualification", "Post Graduate Qualification") %>%
      as.numeric()
  )

oulad_pca <- oulad %>%
  select(total_clicks, avg_grade, gender, age_band, num_of_prev_attempts) %>%
  mutate(
    total_clicks = log1p(total_clicks), # converts total_clicks to a log scale
    gender = gender %>%
      fct_relevel("M", "F") %>%
      as.numeric(),
    age_band = case_when(
      age_band == "0-35" ~ 1,
      age_band %in% c("35-55", "55<=") ~ 2,
      TRUE ~ NA_real_
    ),
    first_attempt = case_when(
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
col2 = colorRampPalette(c('darkred', 'white', 'darkblue'))

# pca_result$var$coord: variables on the rows; components on the columns
# is.corr = FALSE: treats the entry as a generic matrix
# col = col2(20): generates a 20 color palette from the previously defined col2 gradient
# mar: defines the graph margins
# cl.offset: defines the legend offset
plot1 = corrplot(pca_result$var$coord, is.coord = FALSE, col = col2(20), mar = c(0,0,2,0),
                 cl.offset = 0.2, title = "Bubble Plot of PCA Loadings")
print(plot1)

# fviz_eig(): percentage of variance explained be each component
# addlabels = TRUE: writes percentages on the plot
# ylim = c(0, 100): sets y-axis limits from 0 to 100
plot2 = fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))
print(plot2)

# col.var = "black": sets the colors of arrows and labels
# axes = c(1,2): PC1 on x-aixs; PC2 on y-axis
# select.var = list(contrib = 5): selects up to the five highest-contributing variables for each axis 
plot3 = fviz_pca_var(pca_result, col.var = 'black', axes = c(1,2), 
                     select.var = list(contrib = 5), title = "PCA Correlation Circle")
print(plot3)

plot4 = fviz_pca_ind(pca_result, axes = c(1, 2), col.var = "black", geom.ind = "point",
                     title = "First PCA OULAD")
print(plot4)

plot5 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$gender, title = "First PCA Biplot (Gender)")
print(plot5)

plot6 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$age_band, title = "First PCA Biplot (Age)")
print(plot6)

plot7 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$num_of_prev_attempts, title = "First PCA Biplot (First Attempt)")
print(plot7)

plot8 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$final_result, title = "First PCA Biplot (Final Result)")
print(plot8)

plot9 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$highest_education, title = "First PCA Biplot (Education)")
print(plot9)

plot10 = fviz_pca_biplot(pca_result, axes = c(1, 2), col.var = "black",  geom.ind = "point", 
                        col.ind = oulad$imd_band, title = "First PCA Biplot (IMD Band)")
print(plot10)