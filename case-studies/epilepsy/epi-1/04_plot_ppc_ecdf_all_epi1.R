#! /usr/bin/Rscript --vanilla

# case study 1 (PPC) 
# ECDF plots for posterior predictive checks 

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(brms, Matrix, tidyverse, bayesplot, khroma, patchwork)

# set seed
set.seed(42424242)

# load helper functions
source(here::here("R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_one_ecdf_overlay.R"))

# load comparisons df obtained with epi-1/get_comparisons_incl_plots.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))

plot_ppc_ecdf_model_1 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 1") +
  labs(subtitle = "Model 1") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_2 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 2") +
  labs(subtitle = "Model 2") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_3 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 3") +
  labs(subtitle = "Model 3") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_4 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 4") +
  labs(subtitle = "Model 4") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_5 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 5") +
  labs(subtitle = "Model 5") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_6 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 6") +
  labs(subtitle = "Model 6") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_7 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 7") +
  labs(subtitle = "Model 7") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_8 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 8") +
  labs(subtitle = "Model 8") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_1_8 <- (plot_ppc_ecdf_model_2 | plot_ppc_ecdf_model_1) / (plot_ppc_ecdf_model_4 | plot_ppc_ecdf_model_3) / (plot_ppc_ecdf_model_6 | plot_ppc_ecdf_model_5) / (plot_ppc_ecdf_model_8 | plot_ppc_ecdf_model_7)
plot_ppc_ecdf_model_1_8

save_tikz_plot(plot = plot_ppc_ecdf_model_1_8, 
               width = 5.5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_ppc_ecdf_model_1_8.tex"))

plot_ppc_ecdf_model_9 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 9") +
  labs(subtitle = "Model 9") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_10 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 10") +
  labs(subtitle = "Model 10") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_11 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 11") +
  labs(subtitle = "Model 11") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_12 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 12") +
  labs(subtitle = "Model 12") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_13 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 13") +
  labs(subtitle = "Model 13") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_14 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 14") +
  labs(subtitle = "Model 14") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_15 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 15") +
  labs(subtitle = "Model 15") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_16 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 16") +
  labs(subtitle = "Model 16") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_9_16 <- (plot_ppc_ecdf_model_10 | plot_ppc_ecdf_model_9) / (plot_ppc_ecdf_model_12 | plot_ppc_ecdf_model_11) / (plot_ppc_ecdf_model_14 | plot_ppc_ecdf_model_13) / (plot_ppc_ecdf_model_16 | plot_ppc_ecdf_model_15)
plot_ppc_ecdf_model_9_16

save_tikz_plot(plot = plot_ppc_ecdf_model_9_16, 
               width = 5.5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_ppc_ecdf_model_9_16.tex"))

plot_ppc_ecdf_model_17 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 17") +
  labs(subtitle = "Model 17") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_18 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 18") +
  labs(subtitle = "Model 18") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_19 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 19") +
  labs(subtitle = "Model 19") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_20 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 20") +
  labs(subtitle = "Model 20") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_21 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 21") +
  labs(subtitle = "Model 21") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_22 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 22") +
  labs(subtitle = "Model 22") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_23 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 23") +
  labs(subtitle = "Model 23") +
  theme(legend.position = "none", 
        axis.text.y = element_blank())

plot_ppc_ecdf_model_24 <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = "Model 24") +
  labs(subtitle = "Model 24") +
  theme(legend.position = "none")

plot_ppc_ecdf_model_17_24 <- (plot_ppc_ecdf_model_18 | plot_ppc_ecdf_model_17) / (plot_ppc_ecdf_model_20 | plot_ppc_ecdf_model_19 ) / (plot_ppc_ecdf_model_22 | plot_ppc_ecdf_model_21) / (plot_ppc_ecdf_model_24 | plot_ppc_ecdf_model_23)
plot_ppc_ecdf_model_17_24

# preprint format
save_tikz_plot(plot = plot_ppc_ecdf_model_17_24, 
               width = 5.5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_ppc_ecdf_model_17_24.tex"))

# submission format
save_tikz_plot(plot = plot_ppc_ecdf_model_17_24, 
               width = 5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "submission", "plot_initial_ppc_ecdf_model_17_24.tex"))

################################################################################
# work-in-progress 

# model ids
model_ids <- as.character(paste0("Model_", seq(1:4)))
test <- model_ids[1]
test2 <- model_ids[2]

# results for all 24 models 
for (i in 1:length(model_ids)){
  print(i)
  plot_ppc_ecdf_model_l <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = model_ids[i+1])
  plot_ppc_ecdf_model_r <- get_one_ecdf_overlay(full_comparisons_df_reduced, brms::epilepsy$count, model_char = model_ids[i])
  
  return(paste0("plot_initial_ppc_ecdf_", model_ids[i], "_", model_ids[i+1],".tex"))
  #plot_ppc_ecdf_model_combined <- plot_ppc_ecdf_model_l | plot_ppc_ecdf_model_r
  
  #save_tikz_plot(plot = plot_ppc_ecdf_model_combined, width = 5.5,
  #               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", paste0("plot_initial_ppc_ecdf_", model_ids[i], "_", model_ids[i+1],".tex")))
}