#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, purrr, parallel, brms, Matrix, tidyverse, bayesplot, khroma, patchwork)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))

# load data
full_df_reduced <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "epi-1", "df_with_loos_loobb.rds"))
  
get_one_ecdf_overlay <- function(df, y, model_char = ""){
  # set ggplot theme
  theme_set(theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=7),
                    plot.title = element_text(size=7),
                    axis.title = element_text(size=7),
                    axis.text = element_text(size=7)))
  
  # bayesplot colour scheme
  bayesplot::color_scheme_set("gray")
  
  # get predictions for one model 
  yrep <- df |>
    filter(model_id == model_char) |>
    pull(ypred)
  
  # get model family
  modelfamily <- df |>
    filter(model_id == model_char) |>
    mutate(family = recode(family, "poisson" = "Poisson", "negbinomial" = "Negative Binomial")) |>
    pull(family)
  
  # get model name 
  modelname_long <- df |>
    filter(model_id == model_char) |>
    pull(modelnames)
  
  # remove info on prior for plotting 
  modelname <- substr(modelname_long,1,regexpr(",",modelname_long)-1)
  
  # create plot
  plot <- ppc_ecdf_overlay(y = y, yrep = yrep[[1]][1:100,], discrete = TRUE) +
    #scale_x_continuous(limits=c(0,110)) +
    scale_x_continuous(trans="pseudo_log", 
                       breaks=c(0, 5, 20, 50, 100), 
                       limits=c(0,110)) +
    labs(subtitle = paste0(modelfamily)) +
    theme(legend.position="none")
  
  return(plot)
}

plot_ppc_ecdf_model_22 <- get_one_ecdf_overlay(full_df_reduced, brms::epilepsy$count, model_char = "Model 22")
plot_ppc_ecdf_model_21 <- get_one_ecdf_overlay(full_df_reduced, brms::epilepsy$count, model_char = "Model 21")
plot_ppc_ecdf_model_22_21 <- plot_ppc_ecdf_model_22 | plot_ppc_ecdf_model_21
plot_ppc_ecdf_model_22_21

save_tikz_plot(plot = plot_ppc_ecdf_model_22_21, 
               width = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-1", "plot_ppc_ecdf_model_22_21.tex")
)

cowplot::save_plot(here::here("case-studies", "epilepsy", "figures", "plot_ppc_ecdf_model_22_21.png"), 
                   plot_ppc_ecdf_model_22_21, 
                   base_height = 10, 
                   base_aspect_ratio = 1.5)
