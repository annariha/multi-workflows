#! /usr/bin/Rscript --vanilla

# setup ####
# load packages
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, posterior, ggplot2, patchwork)

# set seed
set.seed(42424242)

# set ggplot theme
fontsize <- 8 
theme_set(theme_classic() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=fontsize),
                  plot.title = element_text(size=fontsize),
                  axis.title = element_text(size=fontsize),
                  axis.text = element_text(size=fontsize), 
                  legend.text = element_text(size=fontsize)))

# helper functions 
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))
source(here::here("R", "save_tikz_plot.R"))

# get loo-objects, comparisons, model fits ####

# load comparison df
full_comparisons_df_wo_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_intloo_reloo.rds"))

# What are the models we would consider indistinguishable just based on elpd diff & se?
df_indist <- full_comparisons_df_wo_intloo_reloo |>
  mutate(modelnames = rownames(full_comparisons_df_wo_intloo_reloo)) |>
  select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id) |>
  filter(elpd_diff + 2*se_diff >= 0)

# free up some space
#rm(full_comparisons_df_wo_intloo_reloo)

# load loo object with default + integrated PSIS-LOO-CV and reloo()
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# helper function to extract vector of point-wise elpds
get_pointwise_elpds <- function(x, loo_objects){
  # loo_objects is a list of lists, i.e., a list of loo objects for several models 
  # modelname is a char string that corresponds to one of the names of the named lists in loo_objects
  loos_one_model <- loo_objects[names(loo_objects) %in% x]
  
  pointwise_elpds <- loos_one_model[[1]]$pointwise |>
    as_tibble() |>
    pull(elpd_loo)
  
  return(pointwise_elpds)
}

# What is the best model?
best_model_name <- df_indist |>
  filter(elpd_diff == 0) |>
  pull(modelnames)

# point-wise elpds for the best model 
pointwise_elpds_best <- get_pointwise_elpds(best_model_name, loos_intloo_reloo)
# point-wise pareto-smoothed elpds for best model
ps_pointwise_elpds_best <- pareto_smooth(pointwise_elpds_best, r_eff = 1, return_k = FALSE)

# point-wise elpds & point-wise differences in elpd compared to the best model 
loos_pointwise_indist_models <- df_indist |>
  mutate(pointwise_elpds = purrr::map(modelnames, ~get_pointwise_elpds(.x, loo_objects = loos_intloo_reloo))) |>
  mutate(pointwise_elpd_diffs = purrr::map(pointwise_elpds, function(x) x - pointwise_elpds_best)) |>
  mutate(ps_pointwise_elpds = purrr::map(pointwise_elpds, ~pareto_smooth(.x, r_eff = 1, return_k = FALSE))) |> 
  mutate(ps_pointwise_elpd_diffs = purrr::map(ps_pointwise_elpds, function(x) x - ps_pointwise_elpds_best))

# Which models have few extreme values in point-wise elpds? Those might drive their large standard errors of the difference in elpd. 

# check: elpd_diff is obtained via sum of point-wise values or (equivalently) mean of the point-wise values multiplied with the number of obs
loos_pointwise_indist_models |> 
  mutate(mean_elpd_diff_check1 = purrr::map_dbl(pointwise_elpd_diffs, function(x) sum(x)), 
         mean_elpd_diff_check2 = purrr::map_dbl(pointwise_elpd_diffs, function(x) mean(x) * NROW(x))) |> 
  select(elpd_diff, mean_elpd_diff_check1, mean_elpd_diff_check2)

# Given these point-wise values, can we reliably obtain estimates for mean elpd? 
# Is the normal approximation valid? 

# add Pareto diagnostics 
loos_pointwise_indist_models <- loos_pointwise_indist_models |>
  mutate(min_ss_elpd = purrr::map_dbl(purrr::map_dbl(purrr::map(pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "min_ss"), ~round(.x, digits = 2))) |>
  mutate(khat_elpd = purrr::map_dbl(purrr::map(pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "khat")) |>
  mutate(khat_threshold_elpd = purrr::map_dbl(purrr::map(pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "khat_threshold")) |>
  mutate(min_ss_ps_elpd = purrr::map_dbl(purrr::map_dbl(purrr::map(ps_pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "min_ss"), ~round(.x, digits = 2))) |>
  mutate(khat_ps_elpd = purrr::map_dbl(purrr::map(ps_pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "khat")) |>
  mutate(khat_threshold_ps_elpd = purrr::map_dbl(purrr::map(ps_pointwise_elpds, ~pareto_diags(.x, r_eff = 1)), "khat_threshold"))

# store row of best model 
best_model_row <- loos_pointwise_indist_models[loos_pointwise_indist_models$modelnames == best_model_name,] |>
  mutate(min_ss_elpd_diff = NA,
         khat_elpd_diff = NA,
         khat_threshold_elpd_diff = NA,
         min_ss_ps_elpd_diff = NA)

# add Pareto diagnostics of differences (excluding the best model)  
loos_pointwise_indist_models_temp <- loos_pointwise_indist_models |>
  filter(modelnames != best_model_name) |>
  mutate(min_ss_elpd_diff = purrr::map_dbl(purrr::map(pointwise_elpd_diffs, ~pareto_diags(.x, r_eff = 1)), "min_ss")) |>
  mutate(khat_elpd_diff = purrr::map_dbl(purrr::map(pointwise_elpd_diffs, ~pareto_diags(.x, r_eff = 1)), "khat")) |>
  mutate(khat_threshold_elpd_diff = purrr::map_dbl(purrr::map(pointwise_elpd_diffs, ~pareto_diags(.x, r_eff = 1)), "khat_threshold")) |>
  mutate(min_ss_ps_elpd_diff = purrr::map_dbl(purrr::map(ps_pointwise_elpd_diffs, ~pareto_diags(.x, r_eff = 1)), "min_ss")) 

# add best model back in 
loos_pointwise_indist_models <- loos_pointwise_indist_models_temp |>
  rbind(best_model_row) |>
  mutate(min_ss_elpd_diff = purrr::map_dbl(min_ss_elpd_diff, ~round(.x, digits = 2))) |>
  mutate(min_ss_ps_elpd_diff = purrr::map_dbl(min_ss_ps_elpd_diff, ~round(.x, digits = 2)))

# plot: point-wise difference for worst model among the 86 models vs elpddiff = 0 ####
plot_df_worst_models <- loos_pointwise_indist_models |>
  filter(elpd_diff < -100) |>
  unnest(cols = c(pointwise_elpds, pointwise_elpd_diffs, ps_pointwise_elpds, ps_pointwise_elpd_diffs)) |>
  group_by(model_id) |>
  mutate(obs_id = row_number())

# get ids of extreme observations
outlier_ids <- brms::epilepsy |>
  filter(count >= mean(count)+4*sd(count)) |>
  pull(obs)

# extract the breaks 
# pretty_breaks <- pretty(plot_df_worst_model$obs_id)

plot_elpddiff_pointwise_worst_models <- ggplot(plot_df_worst_models, aes(x = obs_id, y=pointwise_elpd_diffs, colour=model_id)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = as.numeric(outlier_ids), linetype = "dotted") + 
  scale_x_continuous(breaks = as.numeric(outlier_ids), 
                     labels = as.numeric(outlier_ids)) +
  scale_colour_grey(start = .9, end = 0) +
  xlab("Observation ID") +
  ylab("point-wise difference in elpd") +
  theme(legend.title = element_blank())

# preprint format 
save_tikz_plot(plot = plot_elpddiff_pointwise_worst_models, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_extended_elpddiff_pointwise_worst_models.tex"))

# submission format 
save_tikz_plot(plot = plot_elpddiff_pointwise_worst_models, 
               width = 4.75,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "submission", "plot_extended_elpddiff_pointwise_worst_models.tex"))

################################################################################
# not included #################################################################
qplot(ps_pointwise_elpds_best, pointwise_elpds_best) + geom_abline()

ggplot(data=NULL, aes(x=pointwise_elpds_best)) + 
  geom_histogram(bins=263)

ggplot(data=NULL, aes(x=ps_pointwise_elpds_best)) + 
  geom_histogram(bins=263)

mean(pointwise_elpds_best)
mean(ps_pointwise_elpds_best)

pareto_diags(ps_pointwise_elpds_best)
################################################################################
# Does Pareto-smoothing help? 
length(which(loos_pointwise_indist_models$min_ss_elpd > NROW(brms::epilepsy)))
length(which(loos_pointwise_indist_models$min_ss_ps_elpd > NROW(brms::epilepsy)))
length(which(loos_pointwise_indist_models$min_ss_elpd_diff > NROW(brms::epilepsy)))
length(which(loos_pointwise_indist_models$min_ss_ps_elpd_diff > NROW(brms::epilepsy)))
################################################################################

# check for which models we can obtain reliable estimates for the mean and the normal approx. holds for the mean elpd diff based on pointwise elpds
# models where khat > threshold and min_ss > NROW(brms::epilepsy) for the difference in elpd
loos_problematic_models <- loos_pointwise_indist_models |> 
  filter(khat_elpd_diff > khat_threshold_elpd_diff & min_ss_elpd_diff > NROW(brms::epilepsy)) |> 
  select(model_id, modelnames, elpd_diff, se_diff)

# extract model names for problematic models ####
modelnames_problematic_models <- loos_pointwise_indist_models |> 
  filter(khat_elpd_diff > khat_threshold_elpd_diff & min_ss_elpd_diff > NROW(brms::epilepsy)) |> 
  pull(modelnames)

model_id_problematic_models <- loos_pointwise_indist_models |> 
  filter(khat_elpd_diff > khat_threshold_elpd_diff & min_ss_elpd_diff > NROW(brms::epilepsy)) |> 
  pull(model_id)

readr::write_rds(model_id_problematic_models, here::here("case-studies", "epilepsy", "results", "epi-2", "model_id_problematic_models.rds"))

# plot: elpd difference for filtered models ####
df_plot_elpddiff_filtered <- loos_pointwise_indist_models |>
  filter(!modelnames %in% modelnames_problematic_models) 

plot_elpddiff_filtered <- get_plot_elpddiffs(df_plot_elpddiff_filtered, pointsize = 2) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size=6), legend.position = "none")

plot_elpddiff_filtered

# focus on models with low elpd diff
modelnames_large_se <- loos_pointwise_indist_models |>
  filter(elpd_diff < -30) |>
  pull(modelnames)

# check whether these models are also problematic wrt khat > threshold and min_ss > NROW(brms::epilepsy) for the difference in elpd
which(modelnames_large_se %in% modelnames_problematic_models)

df_plot_elpddiff_large_se <- loos_pointwise_indist_models |>
  filter(!modelnames %in% modelnames_large_se) 

plot_elpddiff_large_se <- get_plot_elpddiffs(df_plot_elpddiff_large_se) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size=5), legend.position = "none")

plot_elpddiff_large_se

################################################################################

# df for plotting 
plot_df <- loos_pointwise_indist_models |>
  # filter for best models and worst models 
  #filter(elpd_diff == 0 | elpd_diff < (-50)) |>
  #select(model_id, modelnames, pointwise_elpds, pointwise_elpd_diffs) |>
  unnest(cols = c(pointwise_elpds, pointwise_elpd_diffs, ps_pointwise_elpds, ps_pointwise_elpd_diffs)) |>
  group_by(model_id) |>
  mutate(obs_id = row_number())

# get ids of extreme observations
outlier_ids <- brms::epilepsy |>
  filter(count >= mean(count)+4*sd(count)) |>
  pull(obs)

# extract the breaks 
pretty_br <- pretty(plot_df$obs_id)

ggplot(plot_df, aes(x = obs_id, y=pointwise_elpds, group = model_id, color = model_id)) + 
  geom_point() + 
  geom_vline(xintercept = as.numeric(outlier_ids), linetype = "dotted") + 
  scale_x_continuous(breaks = c(pretty_br, as.numeric(outlier_ids)), 
                     labels = c(pretty_br, as.numeric(outlier_ids)))

ggplot(plot_df, aes(x = obs_id, y=ps_pointwise_elpds, group = model_id, color = model_id)) + 
  geom_point() + 
  geom_vline(xintercept = as.numeric(outlier_ids), linetype = "dotted") + 
  scale_x_continuous(breaks = c(pretty_br, as.numeric(outlier_ids)), 
                     labels = c(pretty_br, as.numeric(outlier_ids)))

ggplot(plot_df, aes(x = obs_id, y=pointwise_elpd_diffs, group = model_id, color = model_id)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = as.numeric(outlier_ids), linetype = "dotted") + 
  scale_x_continuous(breaks = c(pretty_br, as.numeric(outlier_ids)), 
                     labels = c(pretty_br, as.numeric(outlier_ids)))

ggplot(plot_df, aes(x = obs_id, y=ps_pointwise_elpd_diffs, group = model_id, color = model_id)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = as.numeric(outlier_ids), linetype = "dotted") + 
  scale_x_continuous(breaks = c(pretty_br, as.numeric(outlier_ids)), 
                     labels = c(pretty_br, as.numeric(outlier_ids)))
################################################################################

# point-wise elpds for the best model 
pointwise_elpds_best <- plot_df |> 
  filter(elpd_diff == 0) |>
  pull(pointwise_elpds)

# point-wise elpds for Model 164 (= one of the models with issues due to outliers)
pointwise_elpds_164 <- plot_df |> 
  filter(model_id == "Model 164") |>
  pull(pointwise_elpds)

# histogram of pointwise elpds for Model 164
ggplot(data=NULL, aes(x=pointwise_elpds_164)) + 
  geom_histogram(bins=100)

# Given these point-wise values, can we reliably obtain estimates for mean elpd? 
# Is the normal approximation valid? 

# get diagnostics for Pareto smoothing the tail draws of the point-wise elpds by replacing tail draws by order statistics of a generalized Pareto distribution fit to the tail(s)
pareto_diags(pointwise_elpds_164, tail="left", r_eff = 1)

# Does Pareto smoothing have an effect? 
qplot(pointwise_elpds_164, pareto_smooth(pointwise_elpds_164, return_k = FALSE,  r_eff = 1)) + geom_abline()

pareto_smooth(pointwise_elpds_164, return_k = FALSE,  r_eff = 1)
# check whether we can reliably estimate mean difference

# check whether pareto-smoothing of the point-wise elpds helps to estimate the mean difference reliably

# 

################################################################################
# What are the modelnames of the indist. models? 
indist_models_names <- df_indist |>
  pull(modelnames)

# What are the modelnames of the models with big absolute elpd diffs and large se's? 
indist_worst_models_names <- df_indist |>
  filter(elpd_diff < -50) |>
  pull(modelnames)

best_model_id <- df_indist |>
  filter(elpd_diff == 0) |>
  pull(model_id)


# loo object for the best model 
loos_best_model <- loos_intloo_reloo[names(loos_intloo_reloo) %in% best_model_name]

# loo object for indist models 
loos_indist_models <- loos_intloo_reloo[names(loos_intloo_reloo) %in% indist_models_names]

# loo object for worst indist. models 
loo_worst_indist_models <- loos_intloo_reloo[names(loos_intloo_reloo) %in% indist_worst_models_names]

# free up some space 
rm(loos_intloo_reloo)

# turn loo object into pointwise elpd tibble ####

get_pointwise_elpd_df <- function(loo_object){
  # access and reformat pointwise elpds 
  df <- loo_object[[1]]$pointwise |>
    as_tibble() |>
    mutate(obs_id = row_number())
  return(df)
}


# point-wise elpds 
pointwise_vec <- plot_df |>
  filter(model_id == "Model 164") |>
  pull(pointwise_elpds)

hist(pointwise_vec)

pareto_khat(pointwise_vec, tail="left", r_eff = 1)

qplot(pointwise_elpd_124, pareto_smooth(pointwise_elpd_124, return_k = FALSE,  r_eff = 1)) +
  geom_abline()

# not following pareto tail at all 
# tail is not following Pareto distribution here
# this is an outlier, really exceptional obs for this model
# there is no way that we can trust the normal approx. here
# we should only compare reasonable models 
# either: this model is misspecified bc it cannot explain this obs at all
# or: check whether this obs is nonsense & whether there are only a few of such presumable data entry error if we think that the model is reasonable 
# here: our expertise is not sufficient to decide to exclude obs
# -> we would exclude those models as misspecified 

# 
# invalid comparison 

# What to do? 
# 1. data entry error
# 2. 

# cases with high Pareto k value s

# for best model
hist(loos_pointwise_best_model$elpd_loo)

pareto_khat(loos_pointwise_best_model$elpd_loo, r_eff = 1)

pareto_diags(loos_pointwise_best_model$elpd_loo)
# 28 obs needed to get reliable estimate for the mean 
# normal approx. works 

diff_pointwise <- pareto_smooth(loos_pointwise_best_model$elpd_loo, return_k = FALSE,  r_eff = 1) - pareto_smooth(pointwise_vec, return_k = FALSE,  r_eff = 1)
hist(diff_pointwise)

pareto_khat(diff_pointwise, tail = "right", r_eff = 1)

pareto_diags(diff_pointwise,tail = "right", r_eff = 1)
# -> this comparison is not valid and Pareto smoothing does not help to make it a valid comparison 
# elpd diff
# now tail is so extreme
# thus, se does not make sense
# we need another reason to discard the worse model 
# -> one reason could be that it does not 


# ecdf difference plot between fitted and observed pointwise elpds 
# 1. get PIT values 
# 2. tail size, we want diagnostics for gdp fit 

# tail threshold

pointwise_elpd_124 <- loos_pointwise_indist_models |>
  filter(model_id == "Model 124") |>
  pull(pointwise_elpds) |>
  unlist()

pointwise_elpd_138 <- loos_pointwise_indist_models |>
  filter(model_id == "Model 138") |>
  pull(pointwise_elpds) |>
  unlist()


# point-wise elpds for best model 
loos_pointwise_best_model <- get_pointwise_elpd_df(loos_best_model)

ggplot(loos_pointwise_best_model, aes(x = obs_id, y=elpd_loo)) + 
  geom_point() + 
  geom_line(aes(y = mean(elpd_loo))) +
  geom_line(aes(y = mean(elpd_loo) - sd(elpd_loo)), linetype="dashed") +
  geom_line(aes(y = mean(elpd_loo) + sd(elpd_loo)), linetype="dashed") +
  geom_smooth()

ggplot(loos_pointwise_best_model, aes(x = obs_id, y=influence_pareto_k)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0.5, linetype = "dotdash") +
  geom_hline(yintercept = 0.7, linetype = "dashed")

 plot(loos_best_model[[1]], label_points = TRUE)

# PPC for indist. models that have wide range of pointwise elpds ####

# load model fits
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

models_ypred <- models_combs_df |>
  # add ypred for posterior predictive checks
  mutate(ypred = purrr::map(purrr::map(modelfits, pluck), brms::posterior_predict)) |>
  pull(ypred)


# compare modelnames from indist. models to names of the list of loo-objects

# inspect point-wise elpds for outliers ####
test_one_model <- loos_default[[1]]$pointwise |>
  as_tibble() |>
  mutate(obs_id = row_number())

filtered_outliers <- test_one_model |>
  filter(obs_id %in% outlier_ids)

# point-wise elpds for models with too high Pareto khats

# inspect visually
plot(loos_default[[1]], label_points = TRUE)

# extract obs. id for too high Pareto khats
filtered_max_k <- test_one_model |>
  filter(influence_pareto_k > 0.7)

# PPC ####
bayesplot::ppc_intervals_grouped(brms::epilepsy$count,
                                 models_ypred[[1]],
                                 group = brms::epilepsy$Trt)

# histogram of point-wise elpd differences
# side-by-side: Pareto khat plot and similar plot for pointwise elpd differences
# Pareto-k vs. elpd diff

# 1. point-wise elpds (and elpd diffs) for indist. models
# 2. what models have outliers?
# 3. PPC for those models

# Pareto-smoothing for the point-wise elpds ####