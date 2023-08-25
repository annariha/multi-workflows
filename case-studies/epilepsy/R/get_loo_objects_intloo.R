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

# load modelfits and loo-objects 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
loos_randint <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_randint.rds"))

# add model names for models with obs level random intercept ####
modelnames_randint <- models_combs_df |> 
  # only models with obs-level random intercept
  filter(obs != "") |>
  pull(modelnames)
# set names for loo objects
names(loos_randint) <- modelnames_randint

# join with default loos for models without obs-level random intercept ####
modelnames_without_randint <- dplyr::setdiff(names(loos_default), names(loos_randint))
loos_without_randint <- loos_default[modelnames_without_randint]
loos_intloo <- c(loos_randint, loos_without_randint)

# save for later scripts 
readr::write_rds(loos_intloo, here::here("case-studies", "epilepsy", "results", "loos_intloo.rds")) 