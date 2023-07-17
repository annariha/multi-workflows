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

# define function for epred with emmeans()
get_epred_emmeans <- function(modelfit){
  effect_of_interest_draws <- modelfit |>
    emmeans::emmeans(~Trt, epred = TRUE) |>
    emmeans::contrast(method = "revpairwise") |>
    tidybayes::gather_emmeans_draws()
  return(effect_of_interest_draws)
}

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load 
df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds")) 

# posterior average marginal effect of treatment for indistinguishable models with default PSIS-LOO-CV #### 
plot_df_ame_trt_default <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  filter(loo_computation == "default") |>
  #filter(elpd_diff + 1.96*se_diff >= 0) |>
  #filter(elpd_diff >= -4) |>
  filter(elpd_diff + se_diff >= 0) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(model_id, modelnames, modelfits, high_pareto_ks) |>
  mutate(effect_draws = purrr::map(purrr::map(modelfits, pluck), get_epred_emmeans)) |>
  mutate(median_ame_trt = purrr::map_dbl(purrr::map(effect_draws, ".value"), median)) |>
  arrange(median_ame_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  unnest(effect_draws)

# gradient interval plot of indist. models, color indicates high pareto k ####
plot_posterior_ame_trt_default <- ggplot(plot_df_ame_trt_default, aes(x = .value, y = model_id, color = high_pareto_ks)) + 
  stat_halfeye(.width=c(0.5, 0.95), shape=1) +  
  xlim(-6, 3) + 
  xlab("Average marginal effect of treatment") +
  geom_vline(xintercept = 0) + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_ame_trt_default

# posterior average marginal effect of treatment for indist. models with integrated PSIS-LOO-CV #### 
plot_df_ame_trt_intloo <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  filter(loo_computation == "integrated LOO") |>
  #filter(elpd_diff + 1.96*se_diff >= 0) |>
  #filter(elpd_diff >= -4) |>
  filter(elpd_diff + se_diff >= 0) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(model_id, modelnames, modelfits, high_pareto_ks) |>
  mutate(effect_draws = purrr::map(purrr::map(modelfits, pluck), get_epred_emmeans)) |>
  mutate(median_ame_trt = purrr::map_dbl(purrr::map(effect_draws, ".value"), median)) |>
  arrange(median_ame_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  unnest(effect_draws)

# gradient interval plot of indist. models, color indicates high pareto k ####
plot_posterior_ame_trt_intloo <- ggplot(plot_df_ame_trt_intloo, aes(x = .value, y = model_id, color = high_pareto_ks)) + 
  stat_halfeye(.width=c(0.5, 0.95), shape=1) +  
  xlim(-6, 3) + 
  xlab("Average marginal effect of treatment") +
  geom_vline(xintercept = 0) + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_ame_trt_intloo

# combine the two plots with patchwork ####
plot_posterior_ame_trt_indist <- plot_posterior_ame_trt_default | plot_posterior_ame_trt_intloo
plot_posterior_ame_trt_indist
save_tikz_plot(plot = plot_posterior_ame_trt_indist, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_posterior_ame_trt_indist_sediff.tex"))
