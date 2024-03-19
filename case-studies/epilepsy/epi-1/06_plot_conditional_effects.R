#! /usr/bin/Rscript --vanilla

# case study 1 (PPC) 
# conditional effects plots for models with interaction effect

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, purrr, parallel, brms, Matrix, tidyverse, bayesplot, khroma, patchwork)

# load helper functions
source(here::here("R", "save_tikz_plot.R"))

# load comparisons df obtained with epi-1/get_comparisons_epi1.R
full_comparisons_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "full_comparisons_df_reduced.rds"))
# df for plots: model_id, modelfits
plot_df <- full_comparisons_df_reduced |>
  filter(!zBaseTrt == "") |>
  select(model_id, modelnames, modelfits) |>
  # conditional effects plot objects 
  mutate(cond_effects = purrr::map(purrr::map(modelfits, pluck), ~conditional_effects(.x, effects = "zBase:Trt")))

get_one_cond_effects_plot <- function(modelfit, effect_str = "zBase:Trt", fontsize = 8){
  # set ggplot theme
  theme_set(theme_classic() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=fontsize),
                    plot.title = element_text(size=fontsize),
                    axis.title = element_text(size=fontsize),
                    axis.text = element_text(size=fontsize)))
  # model name from brms fit 
  covars = trimws(gsub(".*~","",as.character(modelfit$formula)[1]))
  fam = as.character(modelfit$family)[1]
  # extract prior info
  if (grepl("user", as.character(modelfit$prior)[10]) & grepl("horseshoe", as.character(modelfit$prior)[1])){
    prior = "RHS"
  } else if (grepl("user", as.character(modelfit$prior)[10]) == FALSE){
    prior = "default"
  }
  # build name
  modelname = paste0(fam, "(", covars, "), ", prior)
  # create plot 
  plot <- plot(conditional_effects(modelfit, effects = effect_str), plot = FALSE)[[1]] +
    scale_fill_manual(values = c("0"= "black", "1"= "blue")) +
    scale_color_manual(values = c("0"= "black", "1"= "blue")) +
    labs(title = modelname)
  return(plot) 
}

plot1 <- get_one_cond_effects_plot(plot_df$modelfits[[1]]) +
  theme(legend.position = "none")
plot2 <- get_one_cond_effects_plot(plot_df$modelfits[[2]]) +
  theme(legend.position = "none")
plot3 <- get_one_cond_effects_plot(plot_df$modelfits[[3]]) +
  theme(legend.position = "none")
plot4 <- get_one_cond_effects_plot(plot_df$modelfits[[4]]) +
  theme(legend.position = "none")
plot5 <- get_one_cond_effects_plot(plot_df$modelfits[[5]])
plot6 <- get_one_cond_effects_plot(plot_df$modelfits[[6]])
plot7 <- get_one_cond_effects_plot(plot_df$modelfits[[7]])
plot8 <- get_one_cond_effects_plot(plot_df$modelfits[[8]])

plot_initial_cond_effects <- (plot1 | plot5) / (plot2 | plot6) / (plot3 | plot7) / (plot4 | plot8)

plot_initial_cond_effects

# preprint format
save_tikz_plot(plot_initial_cond_effects, 
               width = 5.5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_initial_cond_effects.tex"))

# submission format 
save_tikz_plot(plot_initial_cond_effects, 
               width = 5,
               height = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "submission", "plot_initial_cond_effects.tex"))
