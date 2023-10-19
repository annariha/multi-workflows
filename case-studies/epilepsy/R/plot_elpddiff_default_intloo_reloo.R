#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, patchwork)

# load scripts
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_loo_bb_weights.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures", "epi-2")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load default and intloo+reloo results ####
full_df_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_default.rds"))
full_df_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo_reloo.rds"))
#full_df_intloo <- readr::readr_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo.rds"))

# create plot of elpd diff +/- se diff for default LOO ####
df_plot_elpddiff_default <- full_df_default |>
  mutate(modelnames = rownames(full_df_default)) |>
  select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id)

plot_elpddiff_all_default <- 
  get_plot_elpddiffs(df_plot_elpddiff_default, 
                     pointsize = 0.9, 
                     subtitle_char = "Default PSIS-LOO-CV", 
                     ylabel_char = "Models") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -50, -250, -1500),
                     limits = c(-1500,3)) +
  theme(axis.text.y = element_blank(), legend.position = "none")

# create plot of remaining set of models for default LOO ####
df_plot_elpddiff_indist_default <- df_plot_elpddiff_default |>
  #filter(elpd_diff + 2*se_diff >= 0) 
  #filter(abs(elpd_diff) < 4) 
  filter(loo_bb_weight > 0.01)

plot_elpddiff_indist_default <- get_plot_elpddiffs(df_plot_elpddiff_indist_default) +
  theme(axis.title.y = element_blank(), legend.position = "none")

# create plot of LOO-BB weights for remaining set of models for default LOO ####
plot_loo_bb_weights_default <- get_plot_loo_bb_weights(df_plot_elpddiff_indist_default) +
  theme(axis.text.y = element_blank())
plot_loo_bb_weights_default

# Combine elpddiff for all and indist. models for default PSIS-LOO-CV ####
plot_elpddiff_all_indist_default <- plot_elpddiff_all_default | plot_elpddiff_indist_default | plot_loo_bb_weights_default
plot_elpddiff_all_indist_default

save_tikz_plot(plot = plot_elpddiff_all_indist_default, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_all_indist_loo_bb_default.tex")
)

# create plot of elpd diff +/- se diff for integrated LOO + reloo() ####
df_plot_elpddiff_intloo_reloo <- full_df_intloo_reloo |>
  mutate(modelnames = rownames(full_df_intloo_reloo)) |>
  select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id)

plot_elpddiff_all_intloo_reloo <- 
  get_plot_elpddiffs(df_plot_elpddiff_intloo_reloo, 
                     pointsize = 0.9,
                     subtitle_char = "Integrated PSIS brute-force LOO-CV", 
                     ylabel_char = "Models") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -50, -250,-1500),
                     limits = c(-1500,3)) +
  theme(axis.text.y = element_blank(), legend.position = "none")

plot_elpddiff_all_intloo_reloo

# create plot of elpd diff +/- se diff for integrated LOO and reloo() ####
df_plot_elpddiff_indist_intloo_reloo <- df_plot_elpddiff_intloo_reloo |>
  #filter(elpd_diff + se_diff >= 0) 
  #filter(abs(elpd_diff) < 4) 
  filter(loo_bb_weight > 0.01)

plot_elpddiff_indist_intloo_reloo <- get_plot_elpddiffs(df_plot_elpddiff_indist_intloo_reloo) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_elpddiff_indist_intloo_reloo

# plot LOO-BB weights for remaining models with integrated PSIS-LOO-CV and reloo() ####
plot_loo_bb_weights_intloo_reloo <- get_plot_loo_bb_weights(df_plot_elpddiff_indist_intloo_reloo) +
  theme(axis.text.y = element_blank())
plot_loo_bb_weights_intloo_reloo

# Combine elpddiff for all and indist. models for for integrated LOO and reloo() ####
plot_elpddiff_all_indist_intloo_reloo <- plot_elpddiff_all_intloo_reloo | plot_elpddiff_indist_intloo_reloo | plot_loo_bb_weights_intloo_reloo
plot_elpddiff_all_indist_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_all_indist_intloo_reloo, 
               width = 6,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_all_indist_loo_bb_intloo_reloo.tex")
)

# Combine all six plots ####
plot_elpddiff_combined_default_intloo_reloo <- (plot_elpddiff_all_default | plot_elpddiff_indist_default | plot_loo_bb_weights_default) / (plot_elpddiff_all_intloo_reloo | plot_elpddiff_indist_intloo_reloo | plot_loo_bb_weights_intloo_reloo)
plot_elpddiff_combined_default_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_combined_default_intloo_reloo,
               width = 6.5,
               height = 5.8,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_loobb_combined_all_indist_default_intloo_reloo.tex"))

# Combine plots for all models for the two different estimation methods ####
plot_elpddiff_all_default_reloo <- plot_elpddiff_all_default | plot_elpddiff_all_intloo_reloo
plot_elpddiff_all_default_reloo

save_tikz_plot(plot = plot_elpddiff_all_default_reloo, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_all_default_reloo.tex")
)

# Combine plots for indistinguishable models for the two different estimation methods ####
plot_elpddiff_indist_reloo <- plot_elpddiff_indist_default | plot_elpddiff_indist_intloo_reloo
plot_elpddiff_indist_reloo

save_tikz_plot(plot = plot_elpddiff_indist_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_indist_reloo.tex")
)