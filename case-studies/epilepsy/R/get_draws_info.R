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

# load functions
source(here::here("case-studies", "epilepsy", "R", "eval_computation.R"))

# load modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# add more info to df with brms and custom functions####
models_combs_df <- tibble(models_combs_df) |>
  mutate(draws_df = purrr::map(purrr::map(modelfits, pluck), posterior::as_draws_df)) |>
  # sampling diagnostics 
  mutate(ndivtrans = purrr::map_dbl(purrr::map(modelfits, pluck), get_ndivtrans)) |>
  mutate(rhattrt = purrr::map_dbl(purrr::map(modelfits, pluck), get_rhat_trt)) |>
  mutate(prophighrhats = purrr::map_dbl(purrr::map(modelfits, pluck), get_prop_rhats)) |>
  mutate(nlowbulkess = purrr::map_dbl(purrr::map(modelfits, pluck), get_n_low_bulk_ess)) |>
  mutate(proplowbulkess = purrr::map_dbl(purrr::map(modelfits, pluck), get_prop_bulkess)) |>
  # number of model parameters
  mutate(nvars = purrr::map_dbl(purrr::map(modelfits, pluck), brms::nvariables))

test <- models_combs_df[2,]$modelfits[[1]]
