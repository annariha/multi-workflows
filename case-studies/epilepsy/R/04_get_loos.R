#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, furrr, purrr, parallel, 
               brms, Matrix, tidyverse, cmdstanr, spatstat)

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load functions
source(here::here("case-studies", "epilepsy", "R", "build_loo.R"))
source(here::here("case-studies", "epilepsy", "R", "build_loglik.R"))

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# default PSIS-LOO-CV for all models ####
tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_default <- models_combs_df |>
  # for testing
  # slice_sample(n = 5) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loos(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE))
toc()
future::plan(sequential)

# set names for loo objects
names(loos_default) <- models_combs_df$modelnames

# store results ####
readr::write_rds(loos_default, here::here("case-studies", "epilepsy", "results", "loos_default_test.rds"))