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

# run script to get combinations dataframe
source(here::here("case-studies", "epilepsy", "R", "get_combinations.R"))

# build df with combinations, names, fits ####
models_combs_df <- combinations_df |>
  mutate(modelnames = apply(combinations_df, 1, build_name))

# fit models ####
tic()
future::plan(multisession)
models_combs_df$modelfits <- combinations_df |>
  # for testing
  #combinations_df[sample(NROW(combinations_df), 5), ] |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_fit(.x, dataset = brms::epilepsy), 
                    .options=furrr_options(seed=TRUE))
toc()

# save modelfits ####
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}
readr::write_rds(models_combs_df, here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))