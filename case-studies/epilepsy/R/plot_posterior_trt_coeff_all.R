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
plot_df_trt_default_all <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  filter(loo_computation == "default") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(draws_df, modelnames, family, model_id, high_pareto_ks, loo_computation) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, pluck), "b_Trt1" )) |>
  mutate(mean_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(mean_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, mean_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_default_all, here::here("case-studies", "epilepsy", "results", "plot_df_trt_default_all.rds"))

# gradient interval plot of all models, color indicates computational issues ####
plot_posterior_trt_default_all <- ggplot(plot_df_trt_default_all, aes(x = posterior_draws_trt, y = model_id, color = high_pareto_ks)) + 
  stat_pointinterval(.width = c(.5, .95)) +
  #xlim(-1.3, 0.8) + 
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0) + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) +  
  theme(axis.text.y=element_text(size=4),
        axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_trt_default_all

save_tikz_plot(plot = plot_posterior_trt_default_all, 
               width = 6.5,
               height = 8.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_posterior_trt_coeff_all.tex"))
