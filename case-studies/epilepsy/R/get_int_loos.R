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
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))
source(here::here("case-studies", "epilepsy", "R", "build_fit.R"))
source(here::here("case-studies", "epilepsy", "R", "build_loglik.R"))

# run script to get combinations dataframe
source(here::here("case-studies", "epilepsy", "R", "get_combinations.R"))

# store modelnames for later use in loo()####
modelnames <- combinations_df |> 
  # only for models with obs-level random intercept
  filter(obs != "") |>
  group_nest(row_number()) |>
  pull(data) |>
  purrr::map_chr(~build_name(.x))

# loo objects with integration ####
tic()
future::plan(multisession, workers = parallel::detectCores() - 2)
loos_obs_randint <- combinations_df |> 
  # only for models with obs-level random intercept
  filter(obs != "") |>
  # for testing, 317.631 sec elapsed
  # slice_sample(n = 2) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loglik_2(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE)) |>
  furrr::future_map(~loo::loo(.x, r_eff = loo::relative_eff(exp(.x))), .options=furrr_options(seed=TRUE))
toc()

# add model names
names(loos_obs_randint) <- modelnames

# join with default loos for models without obs-level random intercept
loos_with_default <- read_rds(here::here("case-studies", "epilepsy", "results", "loos_with_default.rds"))
modelnames_without_obs_randint <- setdiff(names(loos_with_default), names(loos_obs_randint))
loos_without_obs_randint <- loos_with_default[modelnames_without_obs_randint]
loos_all <- c(loos_obs_randint, loos_without_obs_randint)

# compare models with loo & model averaging weights ####
comparison_df_randint = loo::loo_compare(loos_all)

# store results ####
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}
write_rds(loos_obs_randint, here::here("case-studies", "epilepsy", "results", "loos_obs_randint.rds"))