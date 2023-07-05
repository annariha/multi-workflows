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
source(here::here("case-studies", "epilepsy", "R", "build_loglik.R"))

# load modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# int loos for models with obs level random intercept ####
tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_randint <- models_combs_df |> 
  # only models with obs-level random intercept
  filter(obs != "") |>
  # for testing
  # slice_sample(n = 2) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loglik_2(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE)) |>
  furrr::future_map(~loo::loo(.x, r_eff = loo::relative_eff(exp(.x))), .options=furrr_options(seed=TRUE))
toc()

# add model names for models with obs level random intercept ####
modelnames_randint <- models_combs_df |> 
  # only models with obs-level random intercept
  filter(obs != "") |>
  pull(modelnames)

# set names for loo objects
names(loos_randint) <- modelnames_randint

# store intermediate results ####
readr::write_rds(loos_randint, here::here("case-studies", "epilepsy", "results", "loos_randint.rds"))