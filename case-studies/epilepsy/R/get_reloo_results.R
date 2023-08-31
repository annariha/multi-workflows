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

# load data: we need loo-objects and modelfits for the models with remaining high Pareto k-hats

# run script to get loo objects with integrated LOO results & load data 
source(here::here("case-studies", "epilepsy", "R", "get_loo_objects_intloo.R"))
# remove what is not needed here
rm(loos_default, loos_randint, loos_without_randint, modelnames_randint, modelnames_without_randint)

# reduced df for reloo()
df_for_reloo <- tibble(
  # we want to rerun loo if >0 Pareto k-hats are >0.7
  loo_objects = loos_intloo[purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)) > 0], 
  n_high_khat = purrr::map_dbl(purrr::map(loo_objects, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)),
  model_names = names(loo_objects),
  # get the corresponding modelfits 
  model_fits = models_combs_df[which(models_combs_df$modelnames %in% model_names), "modelfits"][[1]]) 

# run reloo
tic()
df_for_reloo_test <- df_for_reloo |>
  # idea: filter for high Pareto khats below 5% and fix computation for those
  # but: only 4 models are left out
  #filter(n_high_khat < (NROW(brms::epilepsy) / 100) * 5) |>
  # for testing
  #slice_sample(n = 2) |>
  mutate(loos_intloo_reloo = furrr::future_pmap(
    list(purrr::map(loo_objects, purrr::pluck), purrr::map(model_fits, purrr::pluck), check = FALSE),
    .f = brms::reloo, 
    .options = furrr_options(seed = TRUE))) |>
  # get number of high khats again as sanity check
  mutate(n_high_khat_reloo = purrr::map_dbl(purrr::map(loos_intloo_reloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)))
toc()

# we only need this new loo-object for loo::compare() etc.
loos_ok_khat <- loos_intloo[purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)) <= 0]
loos_reloo <- purrr::map(df_for_reloo_test$loos_intloo_reloo, pluck)
loos_intloo_reloo <- c(loos_ok_khat, loos_reloo)

readr::write_rds(loos_intloo_reloo, here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# load again 
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# get comparisons
comparison_df_intloo_reloo <- loo::loo_compare(loos_intloo_reloo)

# add sum of Pareto k's > 0.7 for all models with integrated LOO & reloo() ####
comparison_df_intloo_reloo <- merge(comparison_df_intloo_reloo, 
                                    purrr::map_dbl(purrr::map(loos_intloo_reloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)), 
                                    by="row.names") 
# set row names to model names
rownames(comparison_df_intloo_reloo) <- comparison_df_intloo_reloo$Row.names
# select everything despite Row.names
comparison_df_intloo_reloo <- comparison_df_intloo_reloo[2:length(comparison_df_intloo_reloo)]
# set descriptive name for new column 
colnames(comparison_df_intloo_reloo)[ncol(comparison_df_intloo_reloo)] <- "n_high_pareto_ks"

# add MCSE of elpd for all models with integrated LOO & reloo() ####
comparison_df_intloo_reloo <- merge(comparison_df_intloo_reloo, 
                                    purrr::map_dbl(loos_intloo_reloo, loo::mcse_loo), 
                                    by="row.names") 
# set row names to model names
rownames(comparison_df_intloo_reloo) <- comparison_df_intloo_reloo$Row.names
# select everything despite Row.names
comparison_df_intloo_reloo <- comparison_df_intloo_reloo[2:length(comparison_df_intloo_reloo)]
# set descriptive name for new column 
colnames(comparison_df_intloo_reloo)[ncol(comparison_df_intloo_reloo)] <- "mcse_elpd_loo"

# add elpd diff & PBMA weights etc. to df for plotting ####
# turn models df to dataframe and give rownames for merging
models_combs_df <- as.data.frame(models_combs_df)
rownames(models_combs_df) <- models_combs_df$modelnames

# add loo comparison table with integrated LOO & reloo()
df_intloo_reloo = merge(models_combs_df, comparison_df_intloo_reloo, by=0)
# set row names to model names
rownames(df_intloo_reloo) <- df_intloo_reloo$Row.names
# select everything despite Row.names
df_intloo_reloo = df_intloo_reloo[2:length(df_intloo_reloo)]

# PBMA weights with integrated LOO & reloo()
pbma_weights_intloo_reloo = loo_model_weights(loos_intloo_reloo, method="pseudobma")
pbma_df_intloo_reloo = data.frame(pbma_weight=as.numeric(pbma_weights_intloo_reloo), row.names=names(pbma_weights_intloo_reloo))

# add PBMA weights with integrated LOO & reloo()
full_df_intloo_reloo = merge(df_intloo_reloo, pbma_df_intloo_reloo, by=0)
# set row names to model names (again) 
rownames(full_df_intloo_reloo) <- full_df_intloo_reloo$Row.names
# select everything despite Row.names
full_df_intloo_reloo = full_df_intloo_reloo[2:length(full_df_intloo_reloo)]

# add indicator for loo computation
full_df_intloo_reloo <- full_df_intloo_reloo |>
  mutate(loo_computation = rep("integrated LOO, reloo", NROW(full_df_intloo_reloo)))

# store intermediate result for plots ####
readr::write_rds(full_df_intloo_reloo, here::here("case-studies", "epilepsy", "results", "full_df_intloo_reloo.rds"))