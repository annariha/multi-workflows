#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr,
               ggdendro, cowplot)

# run once
# cmdstanr::install_cmdstan()

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# data
dat <- brms::epilepsy 

# create combinations ####

families <- list(poisson = poisson(), 
                 negbinomial = negbinomial())

# colnames_epi <- names(dat)

combinations_df <- expand.grid(
  family = names(families),
  # fixed effects 
  Trt = c("", "Trt"), 
  zBase = c("", "zBase"),
  zAge = c("", "zAge"),
  # random effects 
  patient = c("", "(1 | patient)"),
  visit = c("", "(1 | visit)"),
  obs = c("", "(1 | obs)")
  )

# add interaction effect in the rows where treatment was left out, (i.e., where Trt == "")
combinations_df <- combinations_df %>% 
  mutate(zBaseTrt = factor(
    case_when(
      Trt == "Trt" ~ "",
      Trt == "" ~ "zBase * Trt"))) %>% 
  # reorder to have family and treatment effects first 
  select(family, Trt, zBaseTrt, everything()) %>% 
  # filter out rows with interaction and zBase
  filter(!(zBaseTrt == "zBase * Trt" & combinations_df$zBase == "zBase"))

# create name for each model ####
build_name <- function(row){
  outcome = "count"
  # which cells in the row are not "family" and non-empty?
  in_id <- c(which(names(row) != "family" & row != ""))
  # cells that are included in the formula
  covars <- row[in_id]
  # extract levels for formula
  covars <- as.character(unlist(covars))
  # paste formula
  formula1 = paste(outcome, "~", paste(covars, collapse = "+")) 
  # build name
  name = paste0(row[["family"]], "(", formula1, ")")
  out <- name
}

# for testing
# row <- combinations_df[100,]
# name <- build_name(row)

# add model name 
#combinations_df <- combinations_df %>% 
#  mutate(model_name = apply(combinations_df, 1, build_name)) 

# create brms formulas ####

build_brms_formula <- function(row, ...){
  # turn into brms-formula
  fam = as.character(unlist(row["family"]))
  # not used: name = as.character(unlist(row["model_name"]))
  outcome = "count"
  # which cells in the row are not "family", "model_name" and non-empty?
  in_id <- c(which(names(row) != "family" & names(row) != "model_name" & row != ""))
  # cells that are included in the formula
  covars <- row[in_id]
  # extract levels for formula
  covars <- as.character(unlist(covars))
  # paste formula
  formula_str = paste(outcome, "~", paste(covars, collapse = "+")) 
  # turn string into formula 
  formula = brms::brmsformula(as.formula(formula_str), family=fam)
  out <- formula 
} 

# add model name and brms formula for each combination ####

comb_df <- combinations_df %>% 
  mutate(
    model_name = apply(combinations_df, 1, build_name), 
    formula = apply(combinations_df, 1, build_brms_formula)) 

# compare every column rowwise in df, *1 to turn T/F to 1/0 if value in column different 
# combn() gives combinations of all rows  
df <- as.matrix(combinations_df)
compare <- t(combn(nrow(df), 2, FUN = function(x) df[x[1],]!=df[x[2],])) * 1
vals <- rowSums(compare)

# create distance matrix with zero on diagonal ####

distmatrix <- diag(x = 0, nrow = NROW(combinations_df), ncol = NROW(combinations_df))
rownames(distmatrix) <- rownames(combinations_df)
colnames(distmatrix) <- rownames(combinations_df)

# add edit distance to lower and upper diagonal 
distmatrix[lower.tri(distmatrix)] <- vals
distmatrix[upper.tri(distmatrix)] <- t(distmatrix)[upper.tri(distmatrix)]
#forceSymmetric(distmatrix)

# cluster based on topology ####

hc_edit <- as.dendrogram(hclust(as.dist(distmatrix))) 
hc_edit_dendro <- dendro_data(hc_edit) 

# add model names as labels
dict <- setNames(comb_df$model_name, 1:96)
hc_edit_dendro$labels$label <- sapply(hc_edit_dendro$labels$label, function(x) dict[[as.character(x)]])

# plot dendrogram with edit distances ####

dendrogram_plot_edit <- ggdendrogram(hc_edit_dendro, rotate = TRUE) 

save_plot(here::here("case-studies", "epilepsy", "figures", "dendrogram_edit_epi.png"), 
          dendrogram_plot_edit, 
          base_height = 14, 
          base_aspect_ratio = 1.5)

# fit model for each combination ####

build_fit <- function(row, ...){
  brm(
    build_brms_formula(row), 
    data=epilepsy, 
    file=digest::digest(build_name(row), algo="md5"),
    backend="cmdstanr", silent=2, refresh=0
  ) 
}

#test_row <- combinations_df[1,]
#mod1 <- build_fit(test_row)

# loo: elpd and model comparison ####

build_loo <- function(row, ...){
  # print(build_name(row))
  file_name = paste0(digest::digest(build_name(row), algo="md5"), "_loo.rds")
  if(file.exists(file_name)){
    return(readRDS(file_name))
  }else{
    rv = loo(build_fit(row), model_names=c(build_name(row)))
    saveRDS(rv, file_name)
    return(rv)
  }
} 

#build_loo(test_row)

model_names = apply(combinations_df, 1, build_name)
rownames(combinations_df) <- model_names

# get loo 
tic()
loos = apply(combinations_df, 1, build_loo)
toc()

# compare models 
comparison_df = loo::loo_compare(loos)
# extract pseudo BMA weights
pbma_weights = loo_model_weights(loos, method="pseudobma")
pbma_df = data.frame(pbma_weight=as.numeric(pbma_weights), row.names=names(pbma_weights))
# add loo comparison table 
full_df = merge(combinations_df, comparison_df, by=0)
# set row names to model names
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]
# merge pseudo BMA weights 
full_df = merge(full_df, pbma_df, by=0)
# set row names to model names (again) 
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# get eval distance matrix ####

get_dist <- function(df, col){
  # scale of col
  scale = max(df[,col]) - min(df[,col])
  # get pairwise (rowwise) absolute scaled differences in col
  compare2 <- t(
    combn(
      nrow(df), 
      m = 2, 
      FUN = function(x) abs(as.numeric(df[x[1],col]) - as.numeric(df[x[2],col])/scale)))
  # get distance matrix 
  distmatrix <- diag(x = 0, nrow = NROW(df), ncol = NROW(df))
  rownames(distmatrix) <- rownames(df)
  colnames(distmatrix) <- rownames(df)
  # add eval distance to lower and upper diagonal 
  distmatrix[lower.tri(distmatrix)] <- compare2
  distmatrix[upper.tri(distmatrix)] <- t(distmatrix)[upper.tri(distmatrix)]
  return(distmatrix)
}

dist_m_eval <- get_dist(full_df, "pbma_weight")
# combn(nrow(df), 2) has dimensions sum(1:95) here 

# combine edit and eval distances ####

dist_comb <- dist_m_eval + distmatrix 

# cluster based on topology and metric of interest ####
hc_comb <- hclust(as.dist(dist_comb))
hc_comb_dendro <- dendro_data(as.dendrogram(hc_comb)) 

# add model names as labels
# dict <- setNames(comb_df$model_name, 1:96)
# hc_comb_dendro$labels$label <- sapply(hc_comb_dendro$labels$label, function(x) dict[[as.character(x)]])

# plot dendrogram with edit and eval distances ####
cluster <- cutree(hc_comb, k = 5)

dendrogram_comb <- ggdendrogram(hc_comb_dendro, rotate = TRUE, color=factor(cluster)) 

# This is not great...
ggplot() + 
  geom_segment(data=segment(hc_comb_dendro), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(hc_comb_dendro), aes(x, y, label=label, hjust=0.5, color=factor(cluster)), size=3) +
  coord_flip() + 
  scale_y_continuous()

save_plot(here::here("case-studies", "epilepsy", "figures", "dendrogram_comb_epi.png"), 
          dendrogram_comb, 
          base_height = 14, 
          base_aspect_ratio = 1.5)

# cut dendrogram so there are 4 clusters
cluster <- cutree(hc_comb, k = 5)

full_df_clust <- full_df %>%
  mutate(model_name = rownames(full_df), 
         model_id = factor(1:NROW(full_df))) %>% 
  left_join(
    tibble(
      model_name = names(cluster),
      cluster = factor(cluster)
    )
  ) 

full_df_clust %>%
  ggplot(aes(model_id, pbma_weight)) +
  geom_point(aes(color = cluster)) + 
  theme_bw()

full_df_clust %>%
  ggplot(aes(model_id, elpd_loo)) +
  geom_point(aes(color = cluster)) + 
  theme_bw()

# add posterior results for treatment ####

# sensitivity ####

# What are the "best" models? 
# For simplicity: models that are well-specified and have highest elpd_loo

# What are the worst models? 
# depends on axis of comparison

# Niko's code 

treatment_sampless = apply(full_df, 1, get_posterior_treat)
                           
get_posterior_treat <- function(row){
  return(posterior_samples(build_fit(row))$b_Trt)
}

)
full_df = cbind(full_df, 
                model_name=model_names,
                treatment_mean=as.numeric(lapply(treatment_sampless, mean)),
                treatment_se=as.numeric(lapply(treatment_sampless, sd)),
                treatment_q05=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.05, names=FALSE))),
                treatment_q25=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.25, names=FALSE))),
                treatment_q75=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.75, names=FALSE))),
                treatment_q95=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.95, names=FALSE)))
)
return(full_df)

build_color <- function(row){
  rgb(0., 0., 0., as.numeric(row[["pbma_weight"]]))
  # rgb(0., 0., 0., .1 + .9 * as.numeric(row[["pbma_weight"]]))
}

dendrogram_plot <- function(model_df, col_){
  full_df = build_full_df(model_df)
  distances = build_distancess(model_df, full_df, col_)
  
  hc <- dendsort(as.dendrogram(hclust(as.dist(distances))))
  hc_df = full_df[hc %>% labels,]
  pbma_weights = hc_df$pbma_weights
  max_pbma_weight = max(hc_df$pbma_weight)
  hc_df$pbma_weight <- hc_df$pbma_weight/max_pbma_weight
  # print(cbind(hc_df, pbma_weights=hc_df$pbma_weight/max(hc_df$pbma_weight))$pbma_weights)
  hc_colors = apply(hc_df, 1, build_color)
  hc_df$pbma_weight <- hc_df$pbma_weight*max_pbma_weight
  # print(hc_df$pbma_weights)
  # print(hc_colors)
  
  # hc <- hc %>%
  #   color_branches(k = length(model_df)) %>%
  #   color_labels(k = length(model_df)) %>% 
  #   set("labels_colors", hc_colors)
  
  # hc_data = dendro_data(as.dendrogram(hclust(as.dist(distances))))
  hc_data = dendro_data(hc)
  xx = c(.5, .5 + hc_data$labels$x)
  yy = c(0, cumsum(hc_df$pbma_weight))
  ff <- approxfun(xx, yy)
  hc_data$segments$x <- ff(hc_data$segments$x)
  hc_data$segments$xend <- ff(hc_data$segments$xend)
  hc_data$labels$x <- ff(hc_data$labels$x)
  labels = label(hc_data)
  
  # hc_data
  
  # ggplot() + 
  # ggdendrogram(hclust(as.dist(distances))) + 
  ggplot(segment(hc_data)) + 
    geom_segment(aes(x=y, y=x, xend=yend, yend=xend)) + 
    # scale_colour_manual(hc_colors) +
    geom_text(data=labels,
              aes(label=label, x=0, y=x), hjust=1, vjust=0, color=hc_colors, nudge_y=.01) +
    scale_x_reverse() +
    ylim(0, 1) +
    ylab("cumulative pbma weight") + 
    ggtitle(paste0("Opacity and height reflect pbma weights.\nClusters reflect topology and ", col_, ".\nDummy lines to match other plot\n...\n..\n.")) +
    theme_minimal() + 
    # scale_x_discrete(position = "top")  + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(),
      # axis.text.x = element_blank(), axis.title.x = element_blank(),
      # axis.text.y = element_blank(), axis.title.y = element_blank()
    )
  # return(ggdendrogram(dendsort(hc)))
  # par(mar=c(5,5,5,2.5*(2+length(model_df))))
  # ggplot() +
  #     ggdendrogram(dendsort(hc)) + 
  #     ggtitle("Color is currently non-functional.\nOpacity reflects pbma weights.\nClusters reflect distance in topology and in mean treatment effect.\nNot sure how to change the figure height X.X.")
  # plot(dendsort(hc), horiz=TRUE, xlab="X.X", main="Color is currently non-functional.\nOpacity reflects pbma weights.\nClusters reflect distance in topology and in mean treatment effect.\nNot sure how to change the figure height X.X.")
  # Circular dendrogram
  # circlize_dendrogram(hc,
  #                     labels_track_height = .5,
  #                     dend_track_height = 0.1)
}
treatment_plot <- function(model_df, ccol, scol=ccol){
  full_df = build_full_df(model_df)
  distances = build_distancess(model_df, full_df, ccol)
  hc <- dendsort(as.dendrogram(hclust(as.dist(distances)))) 
  hc_df = full_df[hc %>% labels,]
  # par(mar=c(5,5,5,5))
  p = ggplot() + 
    geom_vline(xintercept=0, alpha=.1) +  
    xlab(scol) + 
    ggtitle("Each dot/rectangle/vertical line represents one model.\n(Shaded) width represents central 5%-95% and 25%-75% intervals.\nDots/vertical lines represent modelwise means.\nShort horizontal lines separate models.\nHeight represent pbma weights.\nSome models have effectively zero height/weight.")
  # scale_color_manual(values=hc_colors)
  left = 0
  for(i in 1:nrow(hc_df)){
    row = hc_df[i,]
    # for(i in 1:20) {
    # for(i in order(full_df$treatment_mean)) {
    #     row = full_df[i, ]
    right = left + row$pbma_weight
    center = .5 * (left + right)
    # color = build_color(row)
    m = row$treatment_mean
    # s = row$treatment_se
    p = p + 
      # geom_point(aes_string(x=center, y=m)) +
      geom_hline(yintercept=center, alpha=.1, color="black") +
      geom_rect(aes_(ymin=left, ymax=right, xmin=row$treatment_q05, xmax=row$treatment_q95), fill="black", alpha=.25) +
      geom_rect(aes_(ymin=left, ymax=right, xmin=row$treatment_q25, xmax=row$treatment_q75), fill="black", alpha=.25) +
      geom_errorbar(aes_(ymin=left, ymax=right, x=m), color="black", width=.01) +
      geom_line(aes_(y=c(left, right), x=c(m,m)), color="black") +
      geom_point(aes_(y=center, x=m), color="black")
    # p = p + geom_rect(aes(xmin=left, xmax=right, ymin=m-s, ymax=m+s, fill=color, alpha=.1))
    left = right
  }
  p + 
    # geom_hline(yintercept=full_df$treatment_mean %*% full_df$pbma_weight) +
    theme_minimal() + 
    ylim(0, 1) +
    # scale_x_discrete(position = "top")  + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(),
      axis.text.y = element_blank(), axis.title.y = element_blank()
    )
}
combined_plot <- function(model_df, ccol, scol){
  full_df = build_full_df(model_df)
  distances = build_distancess(model_df, full_df, ccol)
  hc <- dendsort(as.dendrogram(hclust(as.dist(distances)))) 
  hc_df = full_df[hc %>% labels,]
  
  # DENDROGRAM
  pbma_weights = hc_df$pbma_weights
  max_pbma_weight = max(hc_df$pbma_weight)
  hc_df$pbma_weight <- hc_df$pbma_weight/max_pbma_weight
  # print(cbind(hc_df, pbma_weights=hc_df$pbma_weight/max(hc_df$pbma_weight))$pbma_weights)
  hc_colors = apply(hc_df, 1, build_color)
  hc_df$pbma_weight <- hc_df$pbma_weight*max_pbma_weight
  # print(hc_df$pbma_weights)
  # print(hc_colors)
  
  # hc <- hc %>%
  #   color_branches(k = length(model_df)) %>%
  #   color_labels(k = length(model_df)) %>% 
  #   set("labels_colors", hc_colors)
  
  # hc_data = dendro_data(as.dendrogram(hclust(as.dist(distances))))
  hc_data = dendro_data(hc)
  xx = c(.5, .5 + hc_data$labels$x)
  yy = c(0, cumsum(hc_df$pbma_weight))
  ff <- approxfun(xx, yy)
  hc_data$segments$x <- ff(hc_data$segments$x)
  hc_data$segments$xend <- ff(hc_data$segments$xend)
  hc_data$labels$x <- ff(hc_data$labels$x)
  labels = label(hc_data)
  
  # hc_data
  
  # ggplot() + 
  # ggdendrogram(hclust(as.dist(distances))) + 
  dendrogram_plot = ggplot(segment(hc_data)) + 
    geom_segment(aes(x=y, y=x, xend=yend, yend=xend)) + 
    # scale_colour_manual(hc_colors) +
    geom_text(data=labels,
              aes(label=label, x=0, y=x), hjust=1, vjust=0, color=hc_colors, nudge_y=.01) +
    scale_x_reverse() +
    ylim(0, 1) +
    ylab("cumulative pbma weight") + 
    ggtitle(paste0("Opacity and height reflect pbma weights.\nClusters reflect topology and ", ccol, ".\nDummy lines to match other plot\n...\n..\n.")) +
    theme_minimal() + 
    # scale_x_discrete(position = "top")  + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(),
      axis.text.x = element_text(color="white"), axis.title.x = element_text(color="white"),
      # axis.text.y = element_blank(), axis.title.y = element_blank()
    )
  # par(mar=c(5,5,5,5))
  
  
  # TREATMENT
  
  treatment_plot = ggplot() + 
    geom_vline(xintercept=0, alpha=.1) +  
    xlab(scol) + 
    ggtitle("Each dot/rectangle/vertical line represents one model.\n(Shaded) width represents central 5%-95% and 25%-75% intervals.\nDots/vertical lines represent modelwise means.\nShort horizontal lines separate models.\nHeight represent pbma weights.\nSome models have effectively zero height/weight.")
  # scale_color_manual(values=hc_colors)
  left = 0
  for(i in 1:nrow(hc_df)){
    row = hc_df[i,]
    # for(i in 1:20) {
    # for(i in order(full_df$treatment_mean)) {
    #     row = full_df[i, ]
    right = left + row$pbma_weight
    center = .5 * (left + right)
    # color = build_color(row)
    m = row$treatment_mean
    # s = row$treatment_se
    treatment_plot = treatment_plot + 
      # geom_point(aes_string(x=center, y=m)) +
      geom_hline(yintercept=center, alpha=.1, color="black") +
      geom_rect(aes_(ymin=left, ymax=right, xmin=row$treatment_q05, xmax=row$treatment_q95), fill="black", alpha=.25) +
      geom_rect(aes_(ymin=left, ymax=right, xmin=row$treatment_q25, xmax=row$treatment_q75), fill="black", alpha=.25) +
      geom_errorbar(aes_(ymin=left, ymax=right, x=m), color="black", width=.01) +
      geom_line(aes_(y=c(left, right), x=c(m,m)), color="black") +
      geom_point(aes_(y=center, x=m), color="black")
    # p = p + geom_rect(aes(xmin=left, xmax=right, ymin=m-s, ymax=m+s, fill=color, alpha=.1))
    left = right
  }
  treatment_plot = treatment_plot + 
    # geom_hline(yintercept=full_df$treatment_mean %*% full_df$pbma_weight) +
    theme_minimal() + 
    ylim(0, 1) +
    # scale_x_discrete(position = "top")  + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(),
      axis.text.y = element_blank(), axis.title.y = element_blank()
    )
  
  grid.arrange(dendrogram_plot, treatment_plot, ncol=2)    
}

show_table <- function(model_df, col, cutoff=1e-6){
  full_df = build_full_df(model_df)
  filtered_df = full_df %>% filter(pbma_weight > cutoff) %>% arrange(desc(elpd_diff))
  knitr::kable(filtered_df[c(col, "pbma_weight", "elpd_diff", "se_diff")], digits = 2) %>% 
    add_header_above(data.frame(title=c(paste("Cutoff: pbma_weight > ", cutoff)), span=c(5)))
  # knitr::kable(full_df[order(-full_df$elpd_diff),][c(col, "pbma_weight", "elpd_diff", "se_diff")], digits = 2)
}
show_all <- function(model_df, ccol, scol="treatment_mean"){
  # print(dendrogram_plot(model_df, ccol))
  # print(treatment_plot(model_df, ccol, scol))
  combined_plot(model_df, ccol, scol)
  show_table(model_df, scol)
}
# distances = build_distancess(full_df, "treatment_mean")