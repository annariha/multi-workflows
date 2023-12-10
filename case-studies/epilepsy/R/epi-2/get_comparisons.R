#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, furrr, purrr, parallel, 
               brms, Matrix, tidyverse, cmdstanr, spatstat)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "get_comparisons_df.R"))

# load data ####

# model combinations and modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# turn tibble into dataframe
models_combs_df <- as.data.frame(models_combs_df)
# row names for merging
rownames(models_combs_df) <- models_combs_df$modelnames

# load loo object with default PSIS-LOO-CV
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
# load loo object with default + integrated PSIS-LOO-CV
loos_intloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo.rds"))
# load loo object with default + integrated PSIS-LOO-CV and reloo()
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# comparison df for default PSIS-LOO-CV computed loos ####
full_df_default <- build_comparison_df(models_combs_df, loos_default)

# comparison df for default + integrated PSIS-LOO-CV computed loos ####
full_df_intloo <- build_comparison_df(models_combs_df, loos_intloo)

# comparison df for default + integrated PSIS-LOO-CV and reloo() computed loos ####
full_df_intloo_reloo <- build_comparison_df(models_combs_df, loos_intloo_reloo)

# store results for plotting scripts ####
readr::write_rds(full_df_default, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_default.rds"))
readr::write_rds(full_df_intloo, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo.rds"))
readr::write_rds(full_df_intloo_reloo, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo_reloo.rds"))