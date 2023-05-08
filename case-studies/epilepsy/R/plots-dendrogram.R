# load combinations dataframe 
combinations_df <- read_rds(here::here())
# compare every column rowwise in df ####
df <- as.matrix(combinations_df  |> select(!prior))
# *1 to turn T/F to 1/0 if value in column different 
# combn() gives combinations of all rows 
compare <- t(combn(nrow(df), 2, FUN = function(x) df[x[1],]!=df[x[2],])) * 1
vals <- rowSums(compare)

# create distance matrix with zero on diagonal ####
distmatrix <- diag(x = 0, nrow = NROW(combinations_df), ncol = NROW(combinations_df))
rownames(distmatrix) <- rownames(combinations_df)
colnames(distmatrix) <- rownames(combinations_df)
# add edit distance to lower and upper diagonal 
distmatrix[lower.tri(distmatrix)] <- vals
distmatrix[upper.tri(distmatrix)] <- t(distmatrix)[upper.tri(distmatrix)]

# cluster based on topology ####
hc_edit <- as.dendrogram(hclust(as.dist(distmatrix))) 
hc_edit_dendro <- dendro_data(hc_edit) 

# add model names as labels
dict <- setNames(paste0(comb_df$model_name,", ", comb_df$priors), 1:192)
hc_edit_dendro$labels$label <- sapply(hc_edit_dendro$labels$label, function(x) dict[[as.character(x)]])

# Plots: plot dendrogram with edit distances ####
dendrogram_plot_edit <- ggdendrogram(hc_edit_dendro, rotate = TRUE)  

save_plot(here::here("case-studies", "epilepsy", "figures", "dendrogram_edit_epi.png"), 
          dendrogram_plot_edit, 
          base_height = 19, 
          base_aspect_ratio = 1.5)