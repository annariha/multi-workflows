#! /usr/bin/Rscript --vanilla

# case study 1 (PPC) 
# ECDF plots for posterior predictive checks 

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, purrr, parallel, brms, Matrix, tidyverse, bayesplot, khroma, patchwork)

# set seed
set.seed(42424242)

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_one_ecdf_overlay.R"))

# load comparisons df obtained with epi-1/get_comparisons_incl_plots.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))

# plot: ECDF plots of best Poisson model and corresponding Negative Binomial model ####
plot_ppc_ecdf_model_22 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 22")
plot_ppc_ecdf_model_21 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 21")

plot_ppc_ecdf_model_22_21 <- plot_ppc_ecdf_model_22 | plot_ppc_ecdf_model_21
plot_ppc_ecdf_model_22_21

save_tikz_plot(plot = plot_ppc_ecdf_model_22_21, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_ppc_ecdf_model_22_21.tex")
)