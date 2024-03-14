#! /usr/bin/Rscript --vanilla

# case study 2 
# plot differences in elpd and associated se 

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, ggdist, patchwork)

# load helper functions
source(here::here("R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures", "epi-2")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load default and intloo+reloo results ####
full_comparisons_df_wo_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_default.rds"))
full_comparisons_df_wo_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_intloo_reloo.rds"))

# create plot dfs ####
df_plot_elpddiff_default <- full_comparisons_df_wo_default |>
  mutate(modelnames = rownames(full_comparisons_df_wo_default)) |>
  select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id)

df_plot_elpddiff_indist_default <- df_plot_elpddiff_default |>
  filter(elpd_diff + 2*se_diff >= 0) 

df_plot_elpddiff_intloo_reloo <- full_comparisons_df_wo_intloo_reloo |>
  mutate(modelnames = rownames(full_comparisons_df_wo_intloo_reloo)) |>
  select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id)

df_plot_elpddiff_indist_intloo_reloo <- df_plot_elpddiff_intloo_reloo |>
  filter(elpd_diff + 2*se_diff >= 0) 

model_id_problematic_models <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "model_id_problematic_models.rds"))

df_plot_elpddiff_filtered <- df_plot_elpddiff_indist_intloo_reloo |>
  filter(!model_id %in% model_id_problematic_models)

# plot: elpd diff +/- se diff for all models with default PSIS-LOO-CV ####
plot_elpddiff_all_default <- 
  get_plot_elpddiffs(df_plot_elpddiff_default, 
                     pointsize = 0.9, 
                     subtitle_char = "Default PSIS-LOO-CV", 
                     ylabel_char = "Models") +
  #scale_x_continuous(trans = "pseudo_log", breaks = c(0, -5, -50, -250, -1500), limits = c(-1500,3)) +
  theme(axis.text.y = element_blank())

# plot: elpd diff +/- se diff for indist. models with default PSIS-LOO-CV ####
plot_elpddiff_indist_default <- 
  get_plot_elpddiffs(df_plot_elpddiff_indist_default, pointsize = 2) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size=7))

# Combine elpddiff for all and indist. models for default PSIS-LOO-CV ####
plot_elpddiff_extended_2se_default <- plot_elpddiff_all_default | plot_elpddiff_indist_default 
plot_elpddiff_extended_2se_default

#save_tikz_plot(plot = plot_elpddiff_extended_2se_default, width = 5.5, filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_extended_elpddiff_2se_default.tex"))

# plot: elpd diff +/- se diff for all models with integrated PSIS & brute-force LOO-CV ####
plot_elpddiff_all_intloo_reloo <- 
  get_plot_elpddiffs(df_plot_elpddiff_intloo_reloo, 
                     pointsize = 0.9,
                     subtitle_char = "Integrated PSIS brute-force LOO-CV", 
                     ylabel_char = "Models") +
  #scale_x_continuous(trans = "pseudo_log", breaks = c(0, -5, -50, -250,-1500), limits = c(-1500,3)) +
  theme(axis.text.y = element_blank())

plot_elpddiff_all_intloo_reloo

# plot: elpd diff +/- se diff for indist. models with integrated PSIS & brute-force LOO-CV ####
plot_elpddiff_indist_intloo_reloo <- get_plot_elpddiffs(df_plot_elpddiff_indist_intloo_reloo, pointsize = 2) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

plot_elpddiff_indist_intloo_reloo

# plot: elpd diff +/- se diff for filtered models with valid normal approx. with integrated PSIS & brute-force LOO-CV ####
plot_elpddiff_filtered <- get_plot_elpddiffs(df_plot_elpddiff_filtered, pointsize = 2) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size=5))

plot_elpddiff_filtered

# Combine all five plots ####
plot_elpddiff_extended_2se_filtering_combined <- (plot_elpddiff_all_default | plot_elpddiff_indist_default) / (plot_elpddiff_all_intloo_reloo | plot_elpddiff_indist_intloo_reloo | plot_elpddiff_filtered)
plot_elpddiff_extended_2se_filtering_combined

# preprint format
save_tikz_plot(plot = plot_elpddiff_extended_2se_filtering_combined,
               width = 6,
               height = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_extended_elpddiff_2se_filtering_combined.tex"))

# submission format 
save_tikz_plot(plot = plot_elpddiff_extended_2se_filtering_combined,
               width = 4.9, 
               height = 4.85, 
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "submission", "plot_extended_elpddiff_2se_filtering_combined.tex"))

# not included #################################################################

# Combine all four plots ####
plot_elpddiff_extended_filtering_combined <- (plot_elpddiff_all_default | plot_elpddiff_indist_default) / (plot_elpddiff_all_intloo_reloo | plot_elpddiff_filtered)
plot_elpddiff_extended_filtering_combined

save_tikz_plot(plot = plot_elpddiff_extended_filtering_combined,
               width = 6,
               height = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_extended_elpddiff_filtering_combined.tex"))

source(here::here("case-studies", "epilepsy", "R", "get_plot_loo_bb_weights.R"))

# create plot of LOO-BB weights for remaining set of models for default LOO ####
plot_loo_bb_weights_default <- get_plot_loo_bb_weights(df_plot_elpddiff_indist_default) +
  theme(axis.text.y = element_blank())
plot_loo_bb_weights_default

# plot LOO-BB weights for remaining models with integrated PSIS-LOO-CV and reloo() ####
plot_loo_bb_weights_intloo_reloo <- get_plot_loo_bb_weights(df_plot_elpddiff_indist_intloo_reloo) +
  theme(axis.text.y = element_blank())
plot_loo_bb_weights_intloo_reloo