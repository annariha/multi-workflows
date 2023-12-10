#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, ggdist, patchwork)

# set ggplot theme
theme_set(theme_classic() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=7),
                  plot.title = element_text(size=7),
                  axis.title = element_text(size=7),
                  axis.text = element_text(size=7)))

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_posterior_trt_coeffs.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures", "epi-2")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load plot data obtained with epi-2/plot_dfs_posterior_trt.R
plot_df_trt_all_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_all_default.rds"))
plot_df_trt_filtered <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_filtered.rds"))

# plot: posterior coefficients of treatment for all models with default PSIS-LOO-CV ####
plot_posterior_trt_default_all <- get_plot_posterior_trt_coeffs(plot_df_trt_all_default, ytextsize = 4) +
  theme(legend.position = "none")

plot_posterior_trt_default_all

# save tikz
save_tikz_plot(plot = plot_posterior_trt_default_all, 
               width = 6.5,
               height = 8.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_posterior_trt_all_175.tex"))

# remove model names for combining with other plots after filtering
plot_posterior_trt_all <- get_plot_posterior_trt_coeffs(plot_df_trt_all_default, pointsize = 0.8, alpha = 0.6) +
  theme(axis.text.y = element_blank(), legend.position = "none")

plot_posterior_trt_all

# plot: posterior coefficients of treatment for models with integrated PSIS & brute-force LOO-CV after checking validity of normal approx. ####
plot_posterior_trt_filtered <- ggplot(plot_df_trt_filtered, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
  stat_pointinterval(.width=c(0.5, 0.95)) +
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_posterior_trt_filtered

# plot: posterior coefficients of treatment for all and final filtered set of models ####
plot_posterior_trt_combined <- plot_posterior_trt_all | plot_posterior_trt_filtered
plot_posterior_trt_combined

save_tikz_plot(plot = plot_posterior_trt_combined, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_posterior_trt_default_vs_filtered.tex"))

# not included #################################################################

# plot: posterior coefficients of treatment for indist. models with default PSIS-LOO-CV ####

plot_df_trt_indist_default <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_indist_default.rds"))

plot_posterior_trt_indist2se_default <- 
  ggplot(plot_df_trt_indist_default, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
  stat_pointinterval(.width=c(0.5, 0.95)) +
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_posterior_trt_indist2se_default

save_tikz_plot(plot = plot_posterior_trt_indist2se_default, 
               width = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_posterior_trt_indist2se_default.tex"))

# plot: posterior coefficients of treatment for indist. models with integrated PSIS-LOO-CV and reloo() ####

plot_df_trt_indist_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-2", "plot_df_trt_indist_intloo_reloo.rds"))

plot_posterior_trt_indist2se_intloo_reloo <- 
  ggplot(plot_df_trt_indist_intloo_reloo, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
  stat_pointinterval(.width=c(0.5, 0.95)) +
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_posterior_trt_indist2se_intloo_reloo

save_tikz_plot(plot = plot_posterior_trt_indist2se_intloo_reloo, 
               width = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_posterior_trt_indist2se_intloo_reloo.tex"))
