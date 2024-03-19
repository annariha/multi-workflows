#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, purrr, parallel, brms, Matrix, tidyverse, cmdstanr)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load functions
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_formula_string.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))
source(here::here("case-studies", "epilepsy", "R", "build_fit.R"))

# run script to get combinations dataframe
source(here::here("case-studies", "epilepsy", "R", "01_get_combinations.R"))

# build df with combinations, names, fits ####
models_combs_df <- combinations_df |>
  mutate(modelnames = apply(combinations_df, 1, build_name)) |>
  mutate(formula = apply(combinations_df, 1, build_formula_string))

# if needed: save stancode for each model ####
#helper_data <- combinations_df |>
#  mutate(formula = apply(combinations_df, 1, build_brms_formula)) |>
#  select(formula, family, prior) |>
#  mutate(df = list(brms::epilepsy)) |>
#  mutate(save_model = here::here("case-studies", "epilepsy", "Stan", paste0("model_", 1:NROW(combinations_df), ".stan")))
#purrr::pmap(helper_data, brms::make_stancode)

# workhorse: fit models ####
tic()
future::plan(multisession)
models_combs_df$modelfits <- combinations_df |>
  # for testing
  #slice_sample(n=5)|>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_fit(.x, dataset = brms::epilepsy), 
                    .options=furrr_options(seed=TRUE))
toc()
future::plan(sequential)

# add draws df ####
models_combs_df <- models_combs_df |>
  mutate(model_id = paste0("Model ", row_number())) |>
  mutate(draws_df = purrr::map(purrr::map(modelfits, pluck), posterior::as_draws_df))
         
# save df with modelfits ####
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}
readr::write_rds(models_combs_df, here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))