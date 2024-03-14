#! /usr/bin/Rscript --vanilla

# case study 1
# plot differences in elpd and associated se 

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, brms, Matrix, tidyverse, ggdist, bayesplot, khroma, patchwork)

# load helper functions
source(here::here("R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))

# load comparisons df obtained with epi-1/get_comparisons_epi1.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))
full_comparisons_df_reduced_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced_intloo_reloo.rds"))

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

save_tikz_plot(plot = plot_elpddiff_2se_default, 
               width = 4.75,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "submission", "plot_initial_elpddiff_2se_default.tex")
)

# combine two plots with integrated PSIS & brute-force LOO-CV -> fixing computation does not affect the filtering ####
plot_elpddiff_2se_intloo_reloo <- plot_elpd_diffs_intloo_reloo | plot_elpd_diffs_indist_intloo_reloo

plot_elpddiff_2se_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_2se_intloo_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_elpddiff_2se_intloo_reloo.tex")
)

save_tikz_plot(plot = plot_elpddiff_2se_intloo_reloo, 
               width = 4.75,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "submission", "plot_initial_elpddiff_2se_intloo_reloo.tex")
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