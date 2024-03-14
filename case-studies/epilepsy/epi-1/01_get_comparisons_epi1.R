#! /usr/bin/Rscript --vanilla

# case study 1 (PPC)
# get comparisons df

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, brms, Matrix, tidyverse)

# set seed
set.seed(42424242)

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "build_comparisons_df.R"))

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# load loo-object with default PSIS-LOO-CV obtained with 04_get_loos.R
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
# load loo-object with integrated PSIS-LOO-CV and brute-force LOO CV obtained with 07_get_intloo_reloo_loo_objects.R
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# select models without obs-level effect ####
models_reduced_df <- models_combs_df |>
  filter(obs == "" & patient == "" & visit == "") |>
  # add ypred for posterior predictive checks
  mutate(ypred = purrr::map(purrr::map(modelfits, pluck), brms::posterior_predict))

# get modelnames
modelnames_reduced <- models_reduced_df |>
  pull(modelnames)

# remove big df 
rm(models_combs_df)

# filter loo objects 
loos_reduced <- loos_default[modelnames_reduced]
loos_reduced_intloo_reloo <- loos_intloo_reloo[modelnames_reduced]

# get comparisons df ####
full_comparisons_df_reduced <- build_comparison_df(models_reduced_df, loos_reduced) |>
  select(-c(prior, obs, patient, visit))

full_comparisons_df_reduced_intloo_reloo <- build_comparison_df(models_reduced_df, loos_reduced_intloo_reloo) |>
  select(-c(prior, obs, patient, visit))

# store for other scripts
readr::write_rds(full_comparisons_df_reduced, here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))
readr::write_rds(full_comparisons_df_reduced_intloo_reloo, here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced_intloo_reloo.rds"))