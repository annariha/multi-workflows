#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(tidyverse) 

# load helper functions
source(here::here("case-studies", "R", "save_tikz_plot.R"))

download.file("https://nsiccha.github.io/blog/posts/multiverse-birthday/data/multiverse.csv", 
              destfile=here::here("case-studies", "birthdays", "data", "data.csv"))

df <- read.csv(here::here("case-studies", "birthdays", "data", "data.csv"))

df_plot <- df |>
  mutate(divergent = ifelse(n_divergent > 0, "yes", "no")) |>
  mutate(parametrization = forcats::fct_recode(parametrization, "Adapted parameterisation" = "adapted", "Manual parameterisation" = "noncentered"))

plot_posterior_halloween_effect_all 

plot_posterior_halloween_effect_remaining 

# save as tikzpicture 
save_tikz_plot(plot = plot_posterior_halloween_effect,
               width = 5,
               filename = here::here("case-studies", "birthdays", "figures", "plot_birthdays_elpddiffs_loobb_combined.tex"))