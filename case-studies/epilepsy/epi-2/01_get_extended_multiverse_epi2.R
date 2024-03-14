#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, brms, Matrix, tidyverse)

# set seed
set.seed(42424242)

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "build_comparisons_df.R"))

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# load loo-object with default PSIS-LOO-CV obtained with 04_get_loos.R
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
# load loo-object with integrated PSIS-LOO-CV and brute-force LOO CV obtained with 07_get_intloo_reloo_loo_objects.R
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))
# load comparisons df obtained with epi-1/get_comparisons_incl_plots.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))
  
# get modelnames of models that were filtered out in case study epi-1
modelnames_epi1 <- full_comparisons_df_reduced |>
  filter((elpd_diff + 2*se_diff) < 0) |>
  pull(modelnames)

# look at extended set of models, excluding the models that were filtered out before
models_wo_df <- models_combs_df |>
  filter(!modelnames %in% modelnames_epi1) |>
  # add ypred for posterior predictive checks
  mutate(ypred = purrr::map(purrr::map(modelfits, pluck), brms::posterior_predict))

modelnames_epi2 <- models_wo_df |>
  pull(modelnames)

# filter loo objects 
loos_wo <- loos_default[modelnames_epi2]
loos_wo_intloo_reloo <- loos_intloo_reloo[modelnames_epi2]

# get comparisons df ####
full_comparisons_df_wo_default <- build_comparison_df(models_wo_df, loos_wo)
full_comparisons_df_wo_intloo_reloo <- build_comparison_df(models_wo_df, loos_wo_intloo_reloo)

# store results for plotting scripts ####
readr::write_rds(full_comparisons_df_wo_default, here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_default.rds"))
readr::write_rds(full_comparisons_df_wo_intloo_reloo, here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_intloo_reloo.rds"))