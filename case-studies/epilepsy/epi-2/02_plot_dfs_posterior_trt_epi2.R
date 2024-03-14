#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse)

# load intloo+reloo results ####
full_comparisons_df_wo_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_default.rds"))
full_comparisons_df_wo_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "full_comparisons_df_wo_intloo_reloo.rds"))

# create df: posterior coefficients of treatment for all models with default PSIS-LOO-CV ####
plot_df_trt_all_default <- full_comparisons_df_wo_default |>
  mutate(modelnames = rownames_to_column(full_comparisons_df_wo_default)) |>
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_all_default, here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_all_default.rds"))

# create df: posterior coefficients of treatment for indist. models with default PSIS-LOO-CV ####
plot_df_trt_indist_default <- full_comparisons_df_wo_default |>
  mutate(modelnames = rownames_to_column(full_comparisons_df_wo_default)) |>
  # filtering for models indist. by elpd diff
  filter(elpd_diff + 2*se_diff >= 0) |>
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1")) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_indist_default, here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_indist_default.rds"))

# create df: posterior coefficients of treatment for indist. models with integrated PSIS-LOO-CV and reloo() ####
filtered_comparisons_df <- full_comparisons_df_wo_intloo_reloo |>
  mutate(modelnames = rownames_to_column(full_comparisons_df_wo_intloo_reloo)) |>
  # filtering for models indist. by elpd diff
  filter(elpd_diff + 2*se_diff >= 0)

plot_df_trt_indist_intloo_reloo <- filtered_comparisons_df |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_indist_intloo_reloo, here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_indist_intloo_reloo.rds"))

# create df: posterior coefficients of treatment for models with integrated PSIS & brute-force LOO-CV after checking validity of normal approx. ####
model_id_problematic_models <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "model_id_problematic_models.rds"))
  
plot_df_trt_filtered <- filtered_comparisons_df |>
  filter(!model_id %in% model_id_problematic_models) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_filtered, here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_filtered.rds"))