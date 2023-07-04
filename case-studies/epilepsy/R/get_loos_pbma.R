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
models_combs_df$loos_default <- models_combs_df |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loos(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE))
toc()

# names for loo objects
names(models_combs_df$loos_default) <- models_combs_df$modelnames

# int loos for models with obs level random intercept ####
tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_randint <- models_combs_df |> 
  # only for models with obs-level random intercept
  filter(obs != "") |>
  # for testing
  # slice_sample(n = 2) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loglik_2(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE)) |>
  furrr::future_map(~loo::loo(.x, r_eff = loo::relative_eff(exp(.x))), .options=furrr_options(seed=TRUE))
toc()

# store results ####
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}
write_rds(loos_randint, here::here("case-studies", "epilepsy", "results", "loos_randint.rds"))

# add model names
modelnames_randint <- models_combs_df |> 
  filter(obs != "") |>
  select(modelnames)

names(loos_randint) <- modelnames_randint

# join with default loos for models without obs-level random intercept ####
#loos_with_default <- read_rds(here::here("case-studies", "epilepsy", "results", "loos_with_default.rds"))

modelnames_without_randint <- setdiff(names(models_combs_df$loos_default), names(loos_randint))
loos_without_randint <- loos_with_default[modelnames_without_randint]
loos_int <- c(loos_randint, loos_without_randint)

# join to df using names(loo_int) 

# compare models with loo ####
comparison_df_default = loo::loo_compare(models_combs_df$loos_default)

comparison_df_randint = loo::loo_compare(loos_int)

# get PBMA weights with default loo ####
pbma_weights = loo_model_weights(models_combs_df$loos_default, method="pseudobma")
pbma_df = data.frame(pbma_weight=as.numeric(pbma_weights), row.names=names(pbma_weights))

# get PBMA weights with integrated loo ####

# store results ####

write_rds(loos_obs_randint, here::here("case-studies", "epilepsy", "results", "loos_obs_randint.rds"))