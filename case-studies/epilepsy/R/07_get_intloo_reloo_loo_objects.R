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

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# load loo-object with integrated PSIS-LOO-CV obtained with 06_get_intloo_loo_objects.R
loos_intloo <- readr::read_rds(here::here(("case-studies", "epilepsy", "results", ""))
  
# reduce df to run reloo() for models with remaining high Pareto k-hats 
df_for_reloo <- tibble(
  # we want to rerun loo if >0 Pareto k-hats are still >0.7
  loo_objects = loos_intloo[purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)) > 0], 
  n_high_khat = purrr::map_dbl(purrr::map(loo_objects, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)),
  model_names = names(loo_objects),
  # get the corresponding modelfits 
  model_fits = models_combs_df[which(models_combs_df$modelnames %in% model_names), "modelfits"][[1]]) 

# run reloo ####
tic()
df_for_reloo_test <- df_for_reloo |>
  # for testing
  #slice_sample(n = 2) |>
  mutate(loos_intloo_reloo = furrr::future_pmap(
    list(purrr::map(loo_objects, purrr::pluck), purrr::map(model_fits, purrr::pluck), check = FALSE),
    .f = brms::reloo, 
    .options = furrr_options(seed = TRUE))) |>
  # get number of high khats again as sanity check
  mutate(n_high_khat_reloo = purrr::map_dbl(purrr::map(loos_intloo_reloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)))
toc()

# combine with remaining results obtained with integrated PSIS-LOO-CV ####
# we only need this new loo-object for loo::compare() etc.
loos_ok_khat <- loos_intloo[purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)) <= 0]
loos_reloo <- purrr::map(df_for_reloo_test$loos_intloo_reloo, pluck)
loos_intloo_reloo <- c(loos_ok_khat, loos_reloo)

# save the loo-object 
readr::write_rds(loos_intloo_reloo, here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))