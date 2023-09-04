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

# load modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# default loos for all models ####

# for testing
# models_combs_df <- models_combs_df |>
# dplyr::slice_sample(n = 5)

tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_default <- models_combs_df |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loos(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE))
toc()

# set names for loo objects
names(loos_default) <- models_combs_df$modelnames

# store intermediate results ####
readr::write_rds(loos_default, here::here("case-studies", "epilepsy", "results", "loos_default.rds"))