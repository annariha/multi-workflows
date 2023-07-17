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

# load modelfits and loo objects 
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

# compare models with loo ####
comparison_df_default <- loo::loo_compare(loos_default)
comparison_df_randint <- loo::loo_compare(loos_intloo)

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

# add sum of Pareto k's > 0.7 for all models with integrated LOO ####
comparison_df_randint <- merge(comparison_df_randint, 
                               purrr::map_dbl(purrr::map(loos_intloo, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)), 
                               by="row.names") 
# set row names to model names
rownames(comparison_df_randint) <- comparison_df_randint$Row.names
# select everything despite Row.names
comparison_df_randint <- comparison_df_randint[2:length(comparison_df_randint)]
# set descriptive name for new column 
colnames(comparison_df_randint)[ncol(comparison_df_randint)] <- "n_high_pareto_ks"

# store intermediate results ####
readr::write_rds(comparison_df_default, here::here("case-studies", "epilepsy", "results", "comparison_df_default.rds"))
readr::write_rds(comparison_df_randint, here::here("case-studies", "epilepsy", "results", "comparison_df_randint.rds"))

# re-load
comparison_df_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "comparison_df_default.rds"))
comparison_df_randint <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "comparison_df_randint.rds"))

# add elpd diff & PBMA weights etc. to df for plotting ####

# turn models df to dataframe and give rownames for merging
models_combs_df <- as.data.frame(models_combs_df)
rownames(models_combs_df) <- models_combs_df$modelnames

# add loo comparison table with default LOO
df_default = merge(models_combs_df, comparison_df_default, by=0)
# set row names to model names
rownames(df_default) <- df_default$Row.names
# select everything despite Row.names
df_default = df_default[2:length(df_default)]

# PBMA weights with default LOO 
pbma_weights_default = loo_model_weights(loos_default, method="pseudobma")
pbma_df_default = data.frame(pbma_weight=as.numeric(pbma_weights_default), row.names=names(pbma_weights_default))

# add PBMA weights with default LOO
full_df_default = merge(df_default, pbma_df_default, by=0)
# set row names to model names (again) 
rownames(full_df_default) <- full_df_default$Row.names
# select everything despite Row.names
full_df_default = full_df_default[2:length(full_df_default)]

# add indicator for loo computation
full_df_default <- full_df_default |>
  mutate(loo_computation = rep("default", NROW(full_df_default)))

# add loo comparison table with integrated LOO 
df_intloo = merge(models_combs_df, comparison_df_randint, by=0)
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