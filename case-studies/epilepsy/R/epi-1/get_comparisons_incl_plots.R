#! /usr/bin/Rscript --vanilla

# case study 1 (PPC)
# get comparisons df & elpdiff plots

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, brms, Matrix, tidyverse, ggdist, bayesplot, khroma, patchwork)

# set seed
set.seed(42424242)

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures", "epi-1")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "build_comparisons_df.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_posterior_trt_coeffs.R"))

# load modelfits with evaluated computation obtained with 03_get_draws_info.R
models_combs_df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))
# load loo-object with default PSIS-LOO-CV obtained with 04_get_loos.R
loos_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_default.rds"))
# load loo-object with integrated PSIS-LOO-CV and brute-force LOO CV obtained with 07_get_intloo_reloo_loo_objects.R
loos_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "loos_intloo_reloo.rds"))

# select models without obs-level effect ####
models_reduced_df <- models_combs_df |>
  filter(obs == "" & patient == "" & visit == "") |>
  # add ypred for posterior predictive checks
  mutate(ypred = purrr::map(purrr::map(modelfits, pluck), brms::posterior_predict))

# get modelnames
modelnames_reduced <- models_reduced_df |>
  pull(modelnames)

# remove big df 
rm(models_combs_df)

# filter loo objects 
loos_reduced <- loos_default[modelnames_reduced]
loos_reduced_intloo_reloo <- loos_intloo_reloo[modelnames_reduced]

# get comparisons df ####
full_comparisons_df_reduced <- build_comparison_df(models_reduced_df, loos_reduced) |>
  select(-c(prior, obs, patient, visit))

full_comparisons_df_reduced_intloo_reloo <- build_comparison_df(models_reduced_df, loos_reduced_intloo_reloo) |>
  select(-c(prior, obs, patient, visit))

# store for other scripts
readr::write_rds(full_comparisons_df_reduced, here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))

# plot: elpddiff for default LOO for all models ####
plot_elpd_diffs <- 
  get_plot_elpddiffs(full_comparisons_df_reduced, 
                     subtitle_char = "All models") +
  theme(axis.title.y = element_blank())

plot_elpd_diffs

# plot: elpddiff for intloo+reloo for all models ####
plot_elpd_diffs_intloo_reloo <- 
  get_plot_elpddiffs(full_comparisons_df_reduced_intloo_reloo, 
                     subtitle_char = "Integrated PSIS brute-force LOO-CV") +
  theme(axis.title.y = element_blank())

plot_elpd_diffs_intloo_reloo

# plot: elpddiff for models indistinguishable by elpd diff and se diff ####
df_reduced_indist <- full_comparisons_df_reduced |>
  filter(elpd_diff + 2*se_diff >= 0) 

modelnames_out <- full_comparisons_df_reduced |>
  filter(elpd_diff + 2*se_diff < 0) |>
  pull(modelnames)

plot_elpd_diffs_indist <- 
  get_plot_elpddiffs(df_reduced_indist, subtitle_char = "Filtered set of models")

plot_elpd_diffs_indist

# plot: elpddiff for models indistinguishable by predictive performance after fixing computation ####
df_reduced_indist_intloo_reloo <- full_comparisons_df_reduced_intloo_reloo |>
  filter(elpd_diff + 2*se_diff >= 0) 

plot_elpd_diffs_indist_intloo_reloo <- 
  get_plot_elpddiffs(df_reduced_indist_intloo_reloo, subtitle_char = "Filtered set of models")

plot_elpd_diffs_indist_intloo_reloo

# combine two plots with default PSIS-LOO-CV ####
plot_elpddiff_2se_default <- plot_elpd_diffs | plot_elpd_diffs_indist

plot_elpddiff_2se_default

save_tikz_plot(plot = plot_elpddiff_2se_default, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_elpddiff_2se_default.tex")
)

# combine two plots with integrated PSIS & brute-force LOO-CV -> fixing computation does not affect the filtering ####
plot_elpddiff_2se_intloo_reloo <- plot_elpd_diffs_intloo_reloo | plot_elpd_diffs_indist_intloo_reloo

plot_elpddiff_2se_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_2se_intloo_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_elpddiff_2se_intloo_reloo.tex")
)

# not included #################################################################

source(here::here("case-studies", "epilepsy", "R", "get_plot_loo_bb_weights.R"))

# plot LOO-BB weights for remaining models ####
plot_loo_bb_weights_indist_reduced <- get_plot_loo_bb_weights(df_reduced_indist) +
  theme(axis.text.y = element_blank())
plot_loo_bb_weights_indist_reduced

# PPC plots for two examples (best model, worst of best and best of worst) ####
# Model 18 (worst of the best negbinomial models without issues)
rootogram_model_18 <- get_one_rootogram(full_df_reduced, brms::epilepsy$count, model_char = "Model 18")
# Model 22 (best model, same formula as Model 21 but with negbinomial)
rootogram_model_22 <- get_one_rootogram(full_df_reduced, brms::epilepsy$count, model_char = "Model 22")
# Model 21 (best of the worst Poisson models with issues)
rootogram_model_21 <- get_one_rootogram(full_df_reduced, brms::epilepsy$count, model_char = "Model 21") + 
  theme(axis.title.y = element_blank()) 

rootogram_models_18_21 <- rootogram_model_18 | rootogram_model_21
rootogram_models_18_21

ppc_rootogram_models_22_21 <- rootogram_model_22 | rootogram_model_21
ppc_rootogram_models_22_21

save_tikz_plot(plot = ppc_rootogram_models_22_21, 
               width = 6.5,
               filename = here::here("case-studies", "epilepsy", "figures", "ppc_rootogram_models_22_21.tex")
)

################################################################################

# table testing 
get_formula <- function(df, outcome_colname, covars_colnames){
  formulas <- df |>
    select(model_id, all_of(outcome_colname), all_of(covars_colnames)) |>
    #mutate(rn = row_number()) |>
    pivot_longer(-c(model_id, outcome), values_drop_na = TRUE) |>
    filter(value != "") |>
    group_by(model_id) |>
    mutate(formulas = paste(outcome, "~", paste(value, collapse="+"))) |>
    pivot_wider(id_cols = c(model_id, formulas), names_from = c(name), values_from = c(value)) |>
    select(model_id, formulas)
  return(formulas)
}

# table for case study 1: models specs & elpd (+se) differences & p_loo (+se) & nvars & # of high pareto khats
table_df <- models_combs_df |>
  filter(obs == "" & patient == "" & visit == "") |>
  select(model_id, family, priors, all_of(outcome_colname), all_of(covars_colnames), prophighrhats, proplowtailess, proplowbulkess, ndivtrans, nvars) |>
  # for testing 
  slice_sample(n=5) |>
  pivot_longer(-c(model_id, family, priors, outcome, prophighrhats, proplowtailess, proplowbulkess, ndivtrans, nvars), values_drop_na = TRUE) |>
  filter(value != "") |>
  group_by(model_id) |>
  mutate(formulas = paste(outcome, "~", paste(value, collapse="+"))) |>
  pivot_wider(id_cols = c(model_id, family, priors, outcome, prophighrhats, proplowtailess, proplowbulkess, ndivtrans, nvars, formulas), names_from = c(name), values_from = c(value)) |>
  select(model_id, family, priors, formulas, everything(), -outcome_colname, -covars_colnames)

knitr::kable(table_df, format = "latex")