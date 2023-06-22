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
source(here::here("case-studies", "epilepsy", "R", "build_loo.R"))

# run script to get combinations dataframe
source(here::here("case-studies", "epilepsy", "R", "get_combinations.R"))

# all modelfits ####
#tic()
#future::plan(multisession)
#modelfits <- combinations_df |>
#  group_nest(row_number()) |>
#  pull(data) |>
#  furrr::future_map(~build_fit(.x, dataset = brms::epilepsy), 
#                    .options=furrr_options(seed=TRUE))
#toc()
#write_rds(modelfits, here::here("case-studies", "epilepsy", "results", "modelfits.rds"))

# default loos for all models ####
tic()
future::plan(multisession)
loos_with_default <- combinations_df |>
  # for testing, 126.326 sec elapsed
  # dplyr::slice_sample(n = 5) |>
  group_nest(row_number()) |>
  pull(data) |>
  furrr::future_map(~build_loo(.x, dataset = brms::epilepsy), .options=furrr_options(seed=TRUE))
toc()

# add modelnames 
modelnames_all <- combinations_df |>
  group_nest(row_number()) |>
  pull(data) |>
  purrr::map_chr(~build_name(.x))

names(loos_with_default) <- modelnames_all

# compare models with loo & model averaging weights ####
comparison_df_default = loo::loo_compare(loos_with_default)

# store results ####
filedir = here::here("case-studies", "epilepsy", "results")
if (!dir.exists(filedir)) {dir.create(filedir)}
write_rds(loos_with_default, here::here("case-studies", "epilepsy", "results", "loos_with_default.rds"))