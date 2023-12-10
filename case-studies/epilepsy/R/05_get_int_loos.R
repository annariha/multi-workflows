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

# load helper function
source(here::here("case-studies", "epilepsy", "R", "build_loglik.R"))

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# integrated PSIS-LOO-CV for models with intercept group-level effect for each obs ####
tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_randint <- models_combs_df |> 
  # only models with obs-level effect 
  filter(obs != "") |>
  # for testing
  # slice_sample(n = 2) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loglik_2(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE)) |>
  furrr::future_map(~loo::loo(.x, r_eff = loo::relative_eff(exp(.x))), .options=furrr_options(seed=TRUE))
toc()

# add model names for models with obs-level effect ####
modelnames_randint <- models_combs_df |> 
  filter(obs != "") |>
  pull(modelnames)
# set names for loo objects
names(loos_randint) <- modelnames_randint

# store intermediate results ####
readr::write_rds(loos_randint, here::here("case-studies", "epilepsy", "results", "loos_randint.rds"))