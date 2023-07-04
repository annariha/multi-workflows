#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, purrr, parallel, 
               brms, Matrix, tidyverse, cmdstanr)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load functions
source(here::here("case-studies", "epilepsy", "R", "eval_computation.R"))

# load modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

# add more info to df with brms and custom functions ####
tic()
models_combs_df <- tibble(models_combs_df) |>
  mutate(# sampling diagnostics
         ndivtrans = purrr::map_dbl(purrr::map(modelfits, pluck), get_ndivtrans),
         rhattrt = purrr::map_dbl(purrr::map(modelfits, pluck), get_rhat_trt),
         prophighrhats = purrr::map_dbl(purrr::map(modelfits, pluck), get_prop_rhats),
         nlowbulkess = purrr::map_dbl(purrr::map(modelfits, pluck), get_n_low_bulkess),
         proplowbulkess = purrr::map_dbl(purrr::map(modelfits, pluck), get_prop_bulkess),
         nlowtailess = purrr::map_dbl(purrr::map(modelfits, pluck), get_n_low_tailess),
         proplowtailess = purrr::map_dbl(purrr::map(modelfits, pluck), get_prop_tailess),
         # broad flag for no computational issues
         no_issues = ifelse(ndivtrans == 0 & rhattrt <= 1.01 & prophighrhats <= 0.05 & nlowbulkess == 0 & nlowtailess == 0, 1, 0),
         no_issues_v2 = ifelse(ndivtrans == 0 & rhattrt <= 1.01 & prophighrhats <= 0.05 & proplowbulkess <= 0.05 & proplowtailess <= 0.05, 1, 0),
         # number of model parameters
         nvars = purrr::map_dbl(purrr::map(modelfits, pluck), brms::nvariables))
toc()