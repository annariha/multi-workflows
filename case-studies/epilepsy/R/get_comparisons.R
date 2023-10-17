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

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "get_comparisons_df.R"))

# load data ####

# model combinations and modelfits 
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# turn tibble into dataframe
models_combs_df <- as.data.frame(models_combs_df)
# row names for merging
rownames(models_combs_df) <- models_combs_df$modelnames

# load loo object with default PSIS-LOO-CV
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
# load loo object with default + integrated PSIS-LOO-CV
loos_intloo <- readr::read_rsd(here::here("case-studies", "epilepsy", "results", "loos_intloo.rds"))
# load loo object with default + integrated PSIS-LOO-CV and reloo()
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# comparison df for default PSIS-LOO-CV computed loos ####
full_df_default <- get_comparison_df(models_combs_df, loos_default)

# comparison df for default + integrated PSIS-LOO-CV computed loos ####
full_df_intloo <- get_comparison_df(models_combs_df, loos_intloo)

# comparison df for default + integrated PSIS-LOO-CV and reloo() computed loos ####
full_df_intloo_reloo <- get_comparison_df(models_combs_df, loos_intloo_reloo)

# store results for plotting scripts ####
readr::write_rds(full_df_default, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_default.rds"))
readr::write_rds(full_df_elpddiff_pbma_intloo, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo.rds"))
readr::write_rds(full_df_elpddiff_pbma_intloo_reloo, here::here("case-studies", "epilepsy", "results", "epi-2", "full_df_elpddiff_loobb_intloo_reloo.rds"))

# test for non-zero PBMA weights 
test <- full_df_default |> 
  select(model_id, modelnames, mcse_elpd_loo, elpd_diff, se_diff, pbma_plus_weight, pbma_weight) |>
  mutate(is_zero_pbma_plus = ifelse(pbma_plus_weight < 0.01, "TRUE", "FALSE")) |>
  select(model_id, mcse_elpd_loo, elpd_diff, se_diff, pbma_plus_weight, is_zero_pbma_plus)

# .Machine$double.eps ^ 0.2

test |>
  arrange(elpd_diff) |>
  filter(abs(elpd_diff) < 4) |>
  select(model_id, mcse_elpd_loo, elpd_diff, pbma_plus_weight, is_zero_pbma_plus)

# compare models with loo ####
comparison_df_default <- loo::loo_compare(loos_default)
comparison_df_intloo <- loo::loo_compare(loos_intloo)

# add sum of Pareto k's > 0.7 for all models with default LOO ####
comparison_df_default <- merge(comparison_df_default, 
                               purrr::map_dbl(purrr::map(loos_default, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)), 
                               by="row.names") 
# set row names to model names
rownames(comparison_df_default) <- comparison_df_default$Row.names
# select everything despite Row.names
comparison_df_default <- comparison_df_default[2:length(comparison_df_default)]
# set descriptive name for new column 
colnames(comparison_df_default)[ncol(comparison_df_default)] <- "n_high_pareto_ks"

# add MCSE of elpd for all models with default LOO ####
comparison_df_default <- merge(comparison_df_default, 
                               purrr::map_dbl(loos_default, loo::mcse_loo), 
                               by="row.names") 
# set row names to model names
rownames(comparison_df_default) <- comparison_df_default$Row.names
# select everything despite Row.names
comparison_df_default <- comparison_df_default[2:length(comparison_df_default)]
# set descriptive name for new column 
colnames(comparison_df_default)[ncol(comparison_df_default)] <- "mcse_elpd_loo"

# add sum of Pareto k's > 0.7 for all models with integrated LOO ####
comparison_df_intloo <- merge(comparison_df_intloo, 
                              purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)), 
                              by="row.names") 
# set row names to model names
rownames(comparison_df_intloo) <- comparison_df_intloo$Row.names
# select everything despite Row.names
comparison_df_intloo <- comparison_df_intloo[2:length(comparison_df_intloo)]
# set descriptive name for new column 
colnames(comparison_df_intloo)[ncol(comparison_df_intloo)] <- "n_high_pareto_ks"

# add MCSE of elpd for all models with integrated LOO ####
comparison_df_intloo <- merge(comparison_df_intloo, 
                              purrr::map_dbl(loos_intloo, loo::mcse_loo),
                              by="row.names") 
# set row names to model names
rownames(comparison_df_intloo) <- comparison_df_intloo$Row.names
# select everything despite Row.names
comparison_df_intloo <- comparison_df_intloo[2:length(comparison_df_intloo)]
# set descriptive name for new column 
colnames(comparison_df_intloo)[ncol(comparison_df_intloo)] <- "mcse_elpd_loo"

# store intermediate results ####
readr::write_rds(comparison_df_default, here::here("case-studies", "epilepsy", "results", "comparison_df_default.rds"))
readr::write_rds(comparison_df_intloo, here::here("case-studies", "epilepsy", "results", "comparison_df_intloo.rds"))

# re-load data ####
comparison_df_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "comparison_df_default.rds"))
comparison_df_intloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "comparison_df_intloo.rds"))

# add loo comparison table with default LOO ####
df_default = merge(models_combs_df, comparison_df_default, by=0)
# set row names to model names
rownames(df_default) <- df_default$Row.names
# select everything despite Row.names
df_default = df_default[2:length(df_default)]

# add PBMA+ and PBMA weights with default LOO ####
# likely to be better, based on experiments improves model averaging, less weights close to zero and 1 
pbma_plus_weights_default = loo_model_weights(loos_default, method="pseudobma")
# likely to be more extreme compared to BB=TRUE, when BB is FALSE we should get same ranking order
pbma_weights_default = loo_model_weights(loos_default, method="pseudobma", BB = FALSE)

pbma_df_default = data.frame(pbma_plus_weight=as.numeric(pbma_plus_weights_default),
                             pbma_weight = as.numeric(pbma_weights_default),
                             row.names=names(pbma_weights_default))

# add PBMA weights with default LOO
full_df_default = merge(df_default, pbma_df_default, by=0)
# set row names to model names (again) 
rownames(full_df_default) <- full_df_default$Row.names
# select everything despite Row.names
full_df_default = full_df_default[2:length(full_df_default)]

# add indicator for loo computation
full_df_default <- full_df_default |>
  mutate(loo_computation = rep("default", NROW(full_df_default)))

# add loo comparison table with integrated LOO ####
df_intloo = merge(models_combs_df, comparison_df_intloo, by=0)
# set row names to model names
rownames(df_intloo) <- df_intloo$Row.names
# select everything despite Row.names
df_intloo = df_intloo[2:length(df_intloo)]

# PBMA weights with integrated LOO 
pbma_weights_intloo = loo_model_weights(loos_intloo, method="pseudobma")
pbma_df_intloo = data.frame(pbma_weight=as.numeric(pbma_weights_intloo), row.names=names(pbma_weights_intloo))

# add PBMA weights with integrated LOO
full_df_intloo = merge(df_intloo, pbma_df_intloo, by=0)
# set row names to model names (again) 
rownames(full_df_intloo) <- full_df_intloo$Row.names
# select everything despite Row.names
full_df_intloo = full_df_intloo[2:length(full_df_intloo)]

# add indicator for loo computation
full_df_intloo <- full_df_intloo |>
  mutate(loo_computation = rep("integrated LOO", NROW(full_df_intloo)))

# add the two different comp. methods in one df ####
full_df_elpddiff_pbma <- rbind(full_df_default, full_df_intloo)

# store intermediate result for plots ####
readr::write_rds(full_df_elpddiff_pbma, here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds"))