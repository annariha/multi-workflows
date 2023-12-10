#! /usr/bin/Rscript --vanilla

# case study 1 (PPC) 
# plot posterior results for coefficient of treatment 

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, ggdist, patchwork)

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "get_plot_posterior_trt_coeffs.R"))
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))

# load comparisons df obtained with epi-1/get_comparisons_incl_plots.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))

# plot posterior for coefficient of treatment for all models ####
plot_df <- full_comparisons_df_reduced |>
  mutate(modelnames = rownames_to_column(full_comparisons_df_reduced)) |>
  # arrange according to mean elpd 
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  # create indicator for any Pareto khat's > 0.7
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

plot_posterior_trt_coeff_all_reduced <- ggplot(plot_df, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
  stat_pointinterval(.width=c(0.5, 0.95)) +
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_posterior_trt_coeff_all_reduced

#save_tikz_plot(plot = plot_posterior_trt_coeff_all_reduced, width = 5.5, filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_posterior_trt_coeff_all_reduced.tex"))

# plot posterior coefficient of treatment for indist. models ####
plot_df_indist <- full_comparisons_df_reduced |>
  mutate(modelnames = rownames_to_column(full_comparisons_df_reduced)) |>
  # filtering 
  filter(elpd_diff + 2*se_diff >= 0) |>
  # arrange according to mean elpd 
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  # create indicator for any Pareto khat's > 0.7
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

# set ggplot theme
theme_set(theme_classic() +
            theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=7),
                  plot.title = element_text(size=7),
                  axis.title = element_text(size=7),
                  axis.text = element_text(size=7)))


# half eye plot  
plot_posterior_trt_coeff_indist_reduced <- ggplot(plot_df_indist, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
  stat_halfeye(.width=c(0.5, 0.95)) +
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_trt_coeff_indist_reduced

#save_tikz_plot(plot = plot_posterior_trt_coeff_indist_reduced, width = 5.5, filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_posterior_trt_coeff_indist_reduced.tex"))

# combine the two plots ####
plot_initial_posterior_trt_all_vs_indist <- plot_posterior_trt_coeff_all_reduced | plot_posterior_trt_coeff_indist_reduced
plot_initial_posterior_trt_all_vs_indist

save_tikz_plot(plot = plot_initial_posterior_trt_all_vs_indist, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_posterior_trt_all_vs_indist.tex"))
