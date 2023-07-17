#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, ggdist, patchwork)

# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank()))

# load scripts
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load 
df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds")) 

# posterior treatment effect for indistinguishable models with default PSIS-LOO-CV #### 
plot_df_trt_default <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  filter(loo_computation == "default") |>
  #filter(elpd_diff + 1.96*se_diff >= 0) |>
  #filter(elpd_diff >= -4) |>
  filter(elpd_diff + se_diff >= 0) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks, loo_computation) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_default, here::here("case-studies", "epilepsy", "results", "plot_df_trt_default.rds"))

# gradient interval plot of indist. models, color indicates high pareto k ####
plot_posterior_trt_default <- ggplot(plot_df_trt_default, aes(x = posterior_draws_trt, y = model_id, color = high_pareto_ks)) + 
  stat_halfeye(.width=c(0.5, 0.95), shape=1) +
  #xlim(-1.15, 0.55) + 
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_trt_default

# posterior treatment effect for indistinguishable models with integrated PSIS-LOO-CV #### 
plot_df_trt_intloo <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  filter(loo_computation == "integrated LOO") |>
  #filter(elpd_diff + 1.96*se_diff >= 0) |>
  #filter(elpd_diff >= -4) |>
  filter(elpd_diff + se_diff >= 0) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks, loo_computation) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_intloo, here::here("case-studies", "epilepsy", "results", "plot_df_trt_intloo.rds"))

# gradient interval plot of all models, color indicates computational issues ####
plot_posterior_trt_intloo <- ggplot(plot_df_trt_intloo, aes(x = posterior_draws_trt, y = model_id, color = high_pareto_ks)) + 
  stat_halfeye(.width=c(0.5, 0.95), shape=1) +
  #xlim(-1.15, 0.55) + 
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_trt_intloo

# combine the two plots with patchwork ####
plot_posterior_trt_coeff_indist <- plot_posterior_trt_default | plot_posterior_trt_intloo
plot_posterior_trt_coeff_indist
save_tikz_plot(plot = plot_posterior_trt_coeff_indist, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_posterior_trt_coeff_indist_sediff.tex"))
