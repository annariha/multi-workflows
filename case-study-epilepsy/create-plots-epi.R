#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")

pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, bayesplot, cowplot, RColorBrewer,
               loo, multiverse, priorsense)

# set seed
set.seed(42424242)

# data from brms: https://paul-buerkner.github.io/brms/reference/epilepsy.html
dat <- brms::epilepsy

# load data from results 
multi_epi <- read_rds(here::here("case-study-epilepsy", "results", "multiverse_epi.rds"))
multi_dict_epi <- read_rds( here::here("case-study-epilepsy", "results", "multi_dict_epi.rds"))

# get vector of all names despite "treat"
param_names <-
  map(multi_epi$mod_epi, posterior::variables) %>% 
  unlist() %>% 
  unique() %>%
  stringr::str_remove(., "b_")
param_names[2] <- "treat"

# only plot treatment effect
drop_vec <- param_names[! param_names %in% c("treat")]

# ungrouped 
posterior_plot_ungrouped <- do.call(
  compare_posteriors, 
  list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)")))

save_plot(here::here("case-study-epilepsy", "figures", "post_treat_epi_all.png"), 
          posterior_plot_ungrouped, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# grouped 
to_mods <- as_labeller(c(`TRUE` = "poisson", `FALSE` = "negbinom"))
posterior_plot_treat <- 
  do.call(compare_posteriors, 
          list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)"))) + 
  facet_grid(rows = NULL, 
             vars(model %in% c(1,3,5,7,9,11,13,15,17)), 
             scales = "free", 
             labeller = to_mods)

save_plot(here::here("case-study-epilepsy", "figures", "post_treat_epi.png"), 
          posterior_plot_treat, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# poterior treatment with dotplots

# from tidybayes:

m %>%
  spread_draws() %>%
  ggplot(aes(x = m, y = parameter, color = model, group = model)) +
  stat_dotsinterval(quantiles = 100)

# PPC

y_rep = multi_epi$y_rep
y = data_epi$outcome 

bayesplot$ppc_dens_overlay(y, yrep_poisson[1:50, ])

